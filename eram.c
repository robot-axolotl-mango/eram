/*
 * A simple terminal-based text editor, based on antirez's work on github:
 * https://github.com/antirez/kilo
 */

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <signal.h>
#include <string.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

/*
 * TO CONSIDER: In the future, if I decide to support multiple editor instances
 * at the same time, I can give each editor an ID and include that in the log
 * output. The log can be parsed by a script if it's dumped to a file. This
 * will mean that program initialization needs to be a distinct step from editor
 * initialization.
 */
#ifdef DEBUG
static int LogFD __attribute__((unused)) = STDERR_FILENO;
#define dbg_dprintf(fmt, ...)                                                   \
        do {                                                                    \
                dprintf(LogFD, "[ DEBUG ]: ""%s(): "fmt" (line: %d)\n",         \
                        __func__, ##__VA_ARGS__, __LINE__);                     \
        } while(0)
#else
#define dbg_dprintf(...) do {} while (0)
#endif

#define freenull(ptr)                                                           \
        do {                                                                    \
                free(ptr);                                                      \
                ptr = NULL;                                                     \
        } while (0)

#define ERAM_VERSION            "0.0.1"

/* ============== Syntax highlighting structures and constants ============== */
// syntax.h

enum highlight_flags {
        HL_FLAG_STRINGS = 1u << 0,
        HL_FLAG_NUMBERS = 1u << 1,
};

enum editor_token_cats {
        TOKEN_SL_COMMENT_START,
        TOKEN_ML_COMMENT_START,
        TOKEN_ML_COMMENT_END,
        TOKEN_COUNT,
};

enum syntax_highlight_type {
        HL_TYPE_NORMAL,
        HL_TYPE_NONPRINT,
        HL_TYPE_COMMENT,        /* Single line comment. */
        HL_TYPE_ML_COMMENT,     /* Multi-line comment. */
        HL_TYPE_KEYWORD1,
        HL_TYPE_KEYWORD2,
        HL_TYPE_STRING,
        HL_TYPE_NUMBER,
        HL_TYPE_MATCH,
        HL_TYPE_OUT_OF_RANGE,
};

enum terminal_colors {
        TERM_COLOR_CYAN         = 36,
        TERM_COLOR_YELLOW       = 33,
        TERM_COLOR_GREEN        = 32,
        TERM_COLOR_MAGENTA      = 35,
        TERM_COLOR_RED          = 31,
        TERM_COLOR_BLUE         = 34,
        TERM_COLOR_WHITE        = 37,
};

enum syntax_highlight_default_colors {
        HL_COLOR_COMMENT_DEFAULT        = TERM_COLOR_CYAN,
        HL_COLOR_ML_COMMENT_DEFAULT     = TERM_COLOR_CYAN,
        HL_COLOR_KEYWORD1_DEFAULT       = TERM_COLOR_YELLOW,
        HL_COLOR_KEYWORD2_DEFAULT       = TERM_COLOR_GREEN,
        HL_COLOR_STRING_DEFAULT         = TERM_COLOR_MAGENTA,
        HL_COLOR_NUMBER_DEFAULT         = TERM_COLOR_RED,
        HL_COLOR_MATCH_DEFAULT          = TERM_COLOR_BLUE,
        HL_COLOR_NORMAL_DEFAULT         = TERM_COLOR_WHITE,
};

enum highlight_region {
        HL_REGION_NONE,
        HL_REGION_COMMENT,
        HL_REGION_STRING,
};

enum highlight_boundary {
        HL_BOUNDARY_NONE,
        HL_BOUNDARY_START,
        HL_BOUNDARY_END,
};

struct editor_token_group {
        size_t len[TOKEN_COUNT];
        uint16_t flags;
        enum syntax_highlight_type hl_type[TOKEN_COUNT];
        enum highlight_boundary token_boundary[TOKEN_COUNT];
        char *token[TOKEN_COUNT];
};

struct editor_syntax {
        char **filematch;
        char **keywords;
        char *separator_list;
        struct editor_token_group tokens;
        struct {
                int normal;
                int comment;
                int ml_comment;
                int keyword1;
                int keyword2;
                int string;
                int number;
                int match;
        } colors;
        unsigned int flags;
};

/* ==================== Editor structures and constants ===================== */
// editor.h

#define MAX_STATUS_MSG_LEN      100
#define STATUS_MSG_TIMEOUT      5 /* In seconds */
#define MODIFICATION_NOTICE_MSG "(modified)"
#define STATUS_BAR_HEIGHT       2

#define MAX_CODE_FMT_SIZE       32

#define MAX_RENDER_SIZE         UINT32_MAX
#define DEFAULT_ROW_CAP         8

static volatile sig_atomic_t WindowSizeChanged;

struct editor_row {
        /* Global position within editor matrix */
        int g_pos;

        /* Raw input */
        char *raw_chars;
        size_t raw_chars_size;

        /* Rendered input */
        char *rendered;
        size_t rendered_size;

        /* Highlighting metadata */
        struct {
                /* The highlighting type for each rendered char */
                unsigned char *type;
                struct {
                        /* Metadata state for reference by adjacent rows */
                        bool has_unclosed_token;
                        enum highlight_region region_type;
                        char open_quote_char;
                        char last_char_in_string;
                } ctx;
        } hl;

        /* Previous and next pointers, for reference */
        struct editor_row *prev, *next;
};

struct editor_settings {
        /* TO CONSIDER: This might be more sensible as a uint8_t */
        unsigned int tab_size;
        struct editor_syntax *syntax;
};

struct editor_status {
        char msg[MAX_STATUS_MSG_LEN + 1];
        int msg_len;
        int msg_timeout;
        time_t time;
};

struct editor {
        int cx, cy;
        int row_offset;
        int column_offset;
        int num_screen_rows;
        int num_screen_cols;
        bool in_raw_mode;
        size_t row_cnt;
        size_t rows_cap;
        struct editor_row **rows;
        bool unsaved_changes;
        struct editor_status status;
        int in_fd, out_fd;
        char *filename;
        struct editor_settings settings;
        struct termios orig_termios;
};

enum editor_error {
        E_ERR_GENERAL   = -1,
        E_ERR_ALLOC     = -2,
        E_ERR_FATAL     = -127,
};

/* ------------------------------- User input ------------------------------- */
// input.h

enum key_action {
        KEY_NULL        = 0,            /* NULL */
        CTRL_C          = 3,            /* Ctrl-c */
        CTRL_D          = 4,            /* Ctrl-d */
        CTRL_F          = 6,            /* Ctrl-f */
        CTRL_H          = 8,            /* Ctrl-h */
        TAB             = 9,            /* Tab */
        CTRL_L          = 12,           /* Ctrl+l */
        ENTER           = 13,           /* Enter */
        CTRL_Q          = 17,           /* Ctrl-q */
        CTRL_S          = 19,           /* Ctrl-s */
        CTRL_U          = 21,           /* Ctrl-u */
        ESC             = 27,           /* Escape, equivalent to 1B in hex */
        BACKSPACE       = 127,          /* Backspace */

        /*
         * Antirez: The following are just soft codes, not really reported by
         * the terminal directly.
         */
        ARROW_LEFT      = 1000,
        ARROW_RIGHT,
        ARROW_UP,
        ARROW_DOWN,
        DEL_KEY,
        HOME_KEY,
        END_KEY,
        PAGE_UP,
        PAGE_DOWN
};

static int UserExit;

/* ========================== Syntax Highlighting DB ======================== */
// syntax.h

/*
 * Antirez:
 *
 * In order to add a new syntax, define two arrays with a list of file name
 * matches and keywords. The file name matches are used in order to match a
 * given syntax with a given file name: if a match pattern starts with a dot, it
 * is matched as the last past of the filename, for example ".c". Otherwise the
 * pattern is just searched inside the filenme, like "Makefile").
 *
 * The list of keywords to highlight is just a list of words, however if they
 * a trailing '|' character is added at the end, they are highlighted in a
 * different color, so that you can have two different sets of keywords.
 *
 * Finally add a stanza in the HL_DB global variable with two two arrays of
 * strings, and a set of flags in order to enable highlighting of comments and
 * numbers.
 *
 * The characters for single and multi line comments must be exactly two and
 * must be provided as well (see the C language example).
 *
 * There is no support to highlight patterns currently.
 */

/* This should probably be loadable/configurable from a file */
char *c_cpp_hl_extensions[] = {".c",".h",".cpp",".hpp",".cc",NULL};
char *c_cpp_hl_keywords[] = {
        /* C Keywords */
        "auto","break","case","continue","default","do","else","enum",
        "extern","for","goto","if","#include","register","return","sizeof",
        "static", "struct","switch","typedef","union","volatile","while","NULL",

        /* C++ Keywords */
        "alignas","alignof","and","and_eq","asm","bitand","bitor","class",
        "compl","constexpr","const_cast","deltype","delete","dynamic_cast",
        "explicit","export","false","friend","inline","mutable","namespace",
        "new","noexcept","not","not_eq","nullptr","operator","or","or_eq",
        "private","protected","public","reinterpret_cast","static_assert",
        "static_cast","template","this","thread_local","throw","true","try",
        "typeid","typename","virtual","xor","xor_eq",

        /* C types */
        "int|","long|","double|","float|","char|","unsigned|","signed|",
        "void|","short|","auto|","const|","bool|",NULL
};
char c_cpp_separator_list[] = ",.()+-/*=~%[]";

static const struct editor_token_group c_cpp_tokens = {
                .token[TOKEN_SL_COMMENT_START] = "//",
                .len[TOKEN_SL_COMMENT_START] = sizeof("//") - 1,

                .token[TOKEN_ML_COMMENT_START] = "/*",
                .len[TOKEN_ML_COMMENT_START] = sizeof("/*") - 1,

                .token[TOKEN_ML_COMMENT_END] = "*/",
                .len[TOKEN_ML_COMMENT_END] = sizeof("*/") - 1,
};

struct editor_syntax HighlightDB[] = {
    {
        .filematch = c_cpp_hl_extensions,
        .keywords = c_cpp_hl_keywords,
        .separator_list = c_cpp_separator_list,
        .tokens = c_cpp_tokens,
        .flags = (HL_FLAG_STRINGS | HL_FLAG_NUMBERS),
    }
};

#define HIGHLIGHT_DB_ENTRY_CNT (sizeof(HighlightDB)/sizeof(HighlightDB[0]))

enum ascii_codes {
        ASCII_TILDE = 126,
        ASCII_SPACE = 32,
};

static char EmptyLineChar = ASCII_TILDE;

/* ======================= Low level terminal handling ====================== */
// terminal.c

/* Antirez: Raw mode: 1960 magic shift */
static int editor_enable_raw_mode(struct editor *e)
{
        assert(e);

        if (e->in_raw_mode) return 0;


        if (!isatty(e->in_fd)) goto fatal;
        if (tcgetattr(e->in_fd, &e->orig_termios) == -1) goto fatal;

        struct termios raw = e->orig_termios;

        /* The following notes are from antirez. */

        /*
         * Input modes: no break, no CR to NL, no parity check, no strip char,
         * no start/stop output control.
         */
        raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);

        /* Output modes: Disable post processing */
        raw.c_oflag &= ~OPOST;

        /* Control modes: Set 8 bit chars */
        raw.c_cflag |= CS8;

        /*
         * Local modes: echoing off, canonical off, no extended functions, no
         * signal chars (^Z, ^C)
         */
        raw.c_lflag &= ~(ECHO |ICANON | IEXTEN | ISIG);

        /*
         * Control chars: set return condition: min number of bytes and timer.
         */
        raw.c_cc[VMIN] = 0; /* Return each byte, or zero for timeout */
        raw.c_cc[VTIME] = 1; /* 100 ms timeout (unit is tens of seconds) */

        if (tcsetattr(e->in_fd, TCSAFLUSH, &raw) < 0) goto fatal;

        e->in_raw_mode = true;
        return 0;

fatal:
        errno = ENOTTY;
        return E_ERR_FATAL;
}

static void editor_disable_raw_mode(struct editor *e)
{
        if (e && e->in_raw_mode) {
                tcsetattr(e->in_fd, TCSAFLUSH, &e->orig_termios);
                e->in_raw_mode = false;
        }
}

/* ================= Highlighting selection and application ================= */
// syntax.c

struct parsing_state {
        size_t offset;
        enum highlight_region hl_region;
        enum syntax_highlight_type curr_hl;
        struct {
                char prev_char;
                char quote_char;
        };
        struct {
                enum highlight_boundary hl_boundary;
                size_t curr_token_len;
        };
};

/* ---------------------------- Parser helpers ------------------------------ */

static int get_hl_type_color(enum syntax_highlight_type type,
                             struct editor_settings *settings)
{
        assert(settings);

        if (!settings->syntax) return HL_COLOR_NORMAL_DEFAULT;

        switch (type) {
        case HL_TYPE_NORMAL: return settings->syntax->colors.normal;
        case HL_TYPE_COMMENT: return settings->syntax->colors.comment;
        case HL_TYPE_ML_COMMENT: return settings->syntax->colors.ml_comment;
        case HL_TYPE_KEYWORD1: return settings->syntax->colors.keyword1;
        case HL_TYPE_KEYWORD2: return settings->syntax->colors.keyword2;
        case HL_TYPE_STRING: return settings->syntax->colors.string;
        case HL_TYPE_NUMBER: return settings->syntax->colors.number;
        case HL_TYPE_MATCH: return settings->syntax->colors.match;
        default: return settings->syntax->colors.normal;
        }
}

static bool maybe_my_region(enum highlight_region current_type,
                            enum highlight_region my_type)
{
        return (current_type == my_type || current_type == HL_REGION_NONE);
}

static bool is_separator(const struct editor_settings *settings, int c)
{
        assert(settings);

        return (c == '\0' || isspace(c) ||
                strchr(settings->syntax->separator_list, c));
}

static enum syntax_highlight_type hl_region_to_syntax_hl_type(
                                        enum highlight_region region)
{
        switch (region) {
        case HL_REGION_COMMENT: return HL_TYPE_COMMENT;
        case HL_REGION_STRING: return HL_TYPE_STRING;
        default: return HL_TYPE_NORMAL;
        };

}

/* -------------------------------- Parsers --------------------------------- */

static bool parser_match_nonprint(const struct editor_row *row,
                                  struct parsing_state *st,
          __attribute__((unused)) const struct editor_settings *settings)
{
        assert(row);
        assert(st);

        if (st->offset >= row->rendered_size) return false;
        return !isprint(row->rendered[st->offset]);

}

static int parser_highlight_nonprint(
                        struct editor_row *row,
                        struct parsing_state *st,
__attribute__((unused)) const struct editor_settings *settings)
{
        assert(row);
        assert(st);

        if (st->offset >= row->rendered_size) return E_ERR_GENERAL;
        row->hl.type[st->offset++] = HL_TYPE_NONPRINT;
        return 0;
}

static bool parser_match_token(const struct editor_row *row,
                               struct parsing_state *st,
                               const struct editor_settings *settings)
{
        assert(row);
        assert(st);
        assert(settings);

        if (!maybe_my_region(st->hl_region, HL_REGION_COMMENT) ||
            st->offset >= row->rendered_size) {
                return false;
        }

        char *cursor = row->rendered + st->offset;
        struct editor_token_group *tokens = &settings->syntax->tokens;
        for (size_t i = 0; i < TOKEN_COUNT; i++) {
                char *token = memmem(cursor, row->rendered_size - st->offset,
                                     tokens->token[i], tokens->len[i]);
                if (token == cursor) {
                        st->curr_hl = tokens->hl_type[i];
                        st->curr_token_len = tokens->len[i];
                        st->hl_boundary = tokens->token_boundary[i];
                        return true;
                }
        }
        return false;
}

static int parser_highlight_token(struct editor_row *row,
                                  struct parsing_state *st,
          __attribute__((unused)) const struct editor_settings *settings)
{
        assert(row);
        assert(st);

        if (st->offset >= row->rendered_size) return E_ERR_GENERAL;

        memset(row->hl.type + st->offset, st->curr_hl, st->curr_token_len);
        st->offset += st->curr_token_len;

        if (!maybe_my_region(st->hl_region, HL_REGION_COMMENT)) return 0;

        if (st->hl_boundary == HL_BOUNDARY_START) {
                st->hl_region = HL_REGION_COMMENT;
        } else if (st->hl_boundary == HL_BOUNDARY_END) {
                st->hl_region = HL_REGION_NONE;
                st->curr_hl = HL_TYPE_NORMAL;
        } else {
                assert(false);
                return E_ERR_GENERAL;
        }
        return 0;
}

static bool parser_match_quote_mark(const struct editor_row *row,
                                    struct parsing_state *st,
                                    const struct editor_settings *settings)
{
        assert(row);
        assert(st);
        assert(settings);

        if (!maybe_my_region(st->hl_region, HL_REGION_STRING) ||
            st->offset >= row->rendered_size) {
                return false;
        }

        char *cursor = row->rendered + st->offset;
        bool at_quote_mark = (*cursor == '\'' || *cursor == '"');
        return (at_quote_mark && (settings->syntax->flags & HL_FLAG_STRINGS));
}

static int parser_highlight_string(struct editor_row *row,
                                   struct parsing_state *st,
           __attribute__((unused)) const struct editor_settings *settings)
{
        assert(row);
        assert(st);

        if (st->offset >= row->rendered_size) return E_ERR_GENERAL;

        if (!maybe_my_region(st->hl_region, HL_REGION_STRING)) {
                row->hl.type[st->offset++] = st->curr_hl;
                return 0;
        }
        row->hl.type[st->offset++] = HL_TYPE_STRING;

        char *cursor = row->rendered + st->offset;
        bool at_quote_mark = (*cursor == '\'' || *cursor == '"');
        bool in_string = (st->hl_region == HL_REGION_STRING);
        bool escaped_quote_mark = (at_quote_mark && st->prev_char == '\\');

        bool at_string_start = (at_quote_mark && !in_string);
        bool at_string_end = (in_string && at_quote_mark && !escaped_quote_mark
                              && st->quote_char == *cursor);

        if (at_string_start || at_string_end) {
                st->quote_char = *cursor;
                if (st->hl_region == HL_REGION_STRING) {
                        st->curr_hl = HL_TYPE_NORMAL;
                        st->hl_region = HL_REGION_NONE;
                } else {
                        st->curr_hl = HL_TYPE_STRING;
                        st->hl_region = HL_REGION_STRING;
                }
        }
        st->prev_char = *cursor;

        return 0;
}

static bool parser_match_number(const struct editor_row *row,
                                struct parsing_state *st,
                                const struct editor_settings *settings)
{
        assert(row);
        assert(st);

        if (st->offset >= row->rendered_size) return false;

        return (isdigit(row->rendered[st->offset]) &&
                (settings->syntax->flags & HL_FLAG_NUMBERS) &&
                (st->hl_region == HL_REGION_NONE) &&
                ((row->hl.type[st->offset - 1] == HL_TYPE_NORMAL) &&
                 is_separator(settings, row->rendered[st->offset - 1])));
}

static int parser_highlight_number(struct editor_row *row,
                                   struct parsing_state *st,
                                   const struct editor_settings *settings)
{
        assert(row);
        assert(st);
        assert(settings);

        if (st->offset >= row->rendered_size) return E_ERR_GENERAL;

        char *cursor = row->rendered + st->offset;
        for (;;) {
                row->hl.type[st->offset++] = HL_TYPE_NUMBER;
                cursor++;
                if (!isdigit(*cursor) && *cursor != '.') {
                        if (*--cursor == '.') {
                                row->hl.type[--st->offset] = HL_TYPE_NORMAL;
                        }
                        break;
                }
        }
        return 0;
}

static bool parser_match_keyword(const struct editor_row *row,
                                 struct parsing_state *st,
                                 const struct editor_settings *settings)
{
        assert(row);
        assert(st);
        assert(settings);

        if (st->hl_region != HL_REGION_NONE ||
            st->offset >= row->rendered_size ||
            !is_separator(settings, row->rendered[st->offset - 1])) {
                return false;
        }

        char *cursor = row->rendered + st->offset;
        char **keywords = settings->syntax->keywords;
        for (size_t i = 0; keywords[i]; i++) {
                char *kw = keywords[i];
                int kw_len = strlen(kw);
                bool is_kw_type_2 = (kw[kw_len - 1] == '|');
                if (is_kw_type_2) kw_len--;

                if (st->offset + kw_len >= row->rendered_size) goto no_match;

                if ((memcmp(cursor, kw, kw_len) == 0) &&
                    is_separator(settings, cursor[kw_len])) {
                        st->curr_hl = is_kw_type_2 ? HL_TYPE_KEYWORD1 :
                                                     HL_TYPE_KEYWORD2;
                        st->curr_token_len = kw_len;
                        return true;
                }
        }

no_match:
        return false;
}

static int parser_highlight_keyword(struct editor_row *row,
                                    struct parsing_state *st,
                                    const struct editor_settings *settings)
{
        assert(row);
        assert(st);
        assert(settings);

        if (st->offset >= row->rendered_size) return E_ERR_GENERAL;
        memset(row->hl.type + st->offset, st->curr_hl, st->curr_token_len);
        st->offset += st->curr_token_len;
        st->curr_hl = HL_TYPE_NORMAL;
        return 0;
}

/* ----------------------------- Highlight row -----------------------------  */
// syntax.c

struct parser {
        bool (*match)(const struct editor_row *row,
                      struct parsing_state *st,
                      const struct editor_settings *settings);
        int (*highlight)(struct editor_row *row, struct parsing_state *st,
                         const struct editor_settings *settings);
};

static const struct parser Parsers[] = {
        { parser_match_token, parser_highlight_token },
        { parser_match_quote_mark, parser_highlight_string },
        { parser_match_nonprint, parser_highlight_nonprint },
        { parser_match_number, parser_highlight_number },
        { parser_match_keyword, parser_highlight_keyword },
};

#define NUM_PARSERS (sizeof(Parsers)/sizeof(Parsers[0]))

static int editor_update_row_highlighting(
                struct editor_row *row, struct editor_settings *settings)
{
        assert(row);
        assert(settings);

        if (row->rendered_size == 0) return 0;

        unsigned char *renewed = realloc(row->hl.type, row->rendered_size);
        if (!renewed) return E_ERR_ALLOC;
        row->hl.type = renewed;
        memset(row->hl.type, HL_TYPE_NORMAL, row->rendered_size);

        if (!settings->syntax) return 0;

        struct parsing_state state = {
                .offset = 0,
                .prev_char =
                        row->prev ? row->prev->hl.ctx.last_char_in_string : 0,
                .quote_char = row->prev ? row->prev->hl.ctx.open_quote_char : 0,
                .hl_region = row->prev ? row->prev->hl.ctx.region_type :
                                         HL_REGION_NONE,
        };
        state.curr_hl = hl_region_to_syntax_hl_type(state.hl_region);

        while (state.offset < row->rendered_size) {
                size_t i = 0;
                for (; i < NUM_PARSERS; i++) {
                        if (Parsers[i].match(row, &state, settings)) {
                                if (Parsers[i].highlight(row, &state, settings)
                                    != 0) {
                                        assert(false);
                                        return E_ERR_GENERAL;
                                }
                                break;
                        }
                }

                if (i == NUM_PARSERS) {
                        row->hl.type[state.offset++] = state.curr_hl;
                }
        }
        assert(state.offset == row->rendered_size);

        row->hl.ctx.has_unclosed_token = (state.hl_region != HL_REGION_NONE);
        row->hl.ctx.region_type = state.hl_region;
        row->hl.ctx.last_char_in_string =
                (row->hl.ctx.region_type == HL_REGION_STRING) ?
                state.prev_char : 0;
        row->hl.ctx.open_quote_char =
                (row->hl.ctx.region_type == HL_REGION_STRING) ?
                state.quote_char : 0;

        if (row->hl.ctx.has_unclosed_token && row->next) {
                editor_update_row_highlighting(row->next, settings);
        }
        return 0;
}

/* ======================= Editor Rows Implementation ======================= */
// editor.c

static int editor_row_render(
                struct editor_row *row, struct editor_settings *settings)
{
        assert(row);
        assert(settings);

        unsigned int tabs = 0;
        for (size_t j = 0; j < row->raw_chars_size; j++) {
                if (row->raw_chars[j] == TAB) {
                        tabs++;
               }
        }

        size_t alloc_size =
                1 + row->raw_chars_size + (tabs * settings->tab_size);
        if (alloc_size > MAX_RENDER_SIZE) {
                fprintf(stderr,
                        "Some line of the edited file is too long for eram\n");
                return E_ERR_FATAL;
        }

        free(row->rendered);
        row->rendered = malloc(alloc_size);
        if (!row->rendered) return E_ERR_ALLOC;

        size_t i = 0;
        for (size_t j = 0; j < row->raw_chars_size; j++) {
                if (row->raw_chars[j] == TAB) {
                        do {
                                row->rendered[i++] = ASCII_SPACE;
                        } while ((i % settings->tab_size) != 0);
                } else {
                        row->rendered[i++] = row->raw_chars[j];
                }
        }

        row->rendered_size = i;
        row->rendered[i] = '\0';

        return editor_update_row_highlighting(row, settings);
}

static inline int editor_rows_grow(struct editor *e)
{
        assert(e);

        e->rows_cap = (e->rows_cap) ? (e->rows_cap * 2) : DEFAULT_ROW_CAP;
        size_t new_rows_size = sizeof(e->rows[0]) * e->rows_cap;
        struct editor_row **renewed = realloc(e->rows, new_rows_size);
        if (!renewed) return E_ERR_ALLOC;
        e->rows = renewed;
        return 0;
}

static inline void editor_rows_shift_fwd(struct editor *e, size_t row_pos)
{
        assert(e);

        memmove(e->rows + row_pos + 1, e->rows + row_pos,
                sizeof(e->rows[0]) * (e->row_cnt - row_pos));
        for (size_t i = row_pos + 1; i <= e->row_cnt; i++) e->rows[i]->g_pos++;
}

static inline void editor_row_link(struct editor_row **rows, size_t row_cnt,
                                   struct editor_row *new, size_t row_pos)
{
        assert(rows);
        assert(new);
        assert(row_pos <= row_cnt);

        if (row_pos > 0) {
                new->prev = rows[row_pos - 1];
                rows[row_pos - 1]->next = new;
        }

        if (row_pos < row_cnt) {
                new->next = rows[row_pos + 1];
                rows[row_pos + 1]->prev = new;
        }
}

static int editor_row_init(struct editor_row *row, char *raw, size_t raw_len,
                           size_t row_pos)
{
        assert(row);
        assert(raw);

        if (!raw) return E_ERR_GENERAL;

        row->raw_chars = malloc(raw_len + 1);
        if (!row->raw_chars) return E_ERR_ALLOC;

        row->raw_chars_size = raw_len;
        memcpy(row->raw_chars, raw, row->raw_chars_size);
        row->raw_chars[row->raw_chars_size] = '\0';
        row->g_pos = row_pos;
        row->hl.type = NULL;
        row->hl.ctx.has_unclosed_token = false;
        row->rendered = NULL;
        row->rendered_size = 0;
        row->prev = NULL;
        row->next = NULL;
        return 0;
}

static int editor_row_insert(
                struct editor *e, char *raw, size_t raw_len, size_t row_pos)
{
        assert(e);
        assert(raw);

        if (row_pos > e->row_cnt) return E_ERR_GENERAL;

        if (e->row_cnt + 1 >= e->rows_cap) {
                if (editor_rows_grow(e) != 0) {
                        return E_ERR_ALLOC;
                }
        }

        if (row_pos != e->row_cnt) {
                editor_rows_shift_fwd(e, row_pos);
        }

        struct editor_row *new = malloc(sizeof(*new));
        if (!new) return E_ERR_ALLOC;

        int status = editor_row_init(new, raw, raw_len, row_pos);
        if (status != 0) return status;

        e->rows[row_pos] = new;
        editor_row_link(e->rows, e->row_cnt, new, row_pos);
        /*
         * We increment the row count after the insertion to make sure that the
         * previous checks against the value are coherent and correct.
         */
        e->row_cnt++;
        e->unsaved_changes = true;

        return editor_row_render(new, &e->settings);
}

static int editor_row_append(
                struct editor *e, size_t row_pos, char *s, size_t len)
{
        assert(e);
        assert(s);

        struct editor_row *row = e->rows[row_pos];

        char *tmp = realloc(row->raw_chars, row->raw_chars_size + len + 1);
        if (!tmp) return E_ERR_ALLOC;

        row->raw_chars = tmp;
        memcpy(row->raw_chars + row->raw_chars_size, s, len);
        row->raw_chars_size += len;
        row->raw_chars[row->raw_chars_size] = '\0';
        e->unsaved_changes = true;

        return editor_row_render(row, &e->settings);
}

static int editor_row_delete_char(
                struct editor *e, size_t row_pos, size_t offset)
{
        assert(e);
        assert(row_pos < e->row_cnt);
        assert(offset < e->rows[row_pos]->raw_chars_size);

        struct editor_row *row = e->rows[row_pos];

        memmove(row->raw_chars + offset, row->raw_chars + offset + 1,
                row->raw_chars_size - offset);
        row->raw_chars_size--;
        row->raw_chars[row->raw_chars_size] = '\0';
        e->unsaved_changes = true;

        return editor_row_render(row, &e->settings);
}

/*
 * Antirez: Insert a character at the specified position in a row, moving the
 * remaining chars on the right if needed.
 */
static int editor_row_insert_char(
                struct editor *e, size_t row_pos, size_t offset, int c)
{
        assert(e);
        assert(row_pos < e->row_cnt);

        struct editor_row *row = e->rows[row_pos];

        if (offset > row->raw_chars_size) {
                /*
                 * Antirez: Pad the string with spaces if the insert location is
                 * outside the current length by more than a single character.
                 */
                size_t padlen = offset - row->raw_chars_size;
                 /* Antirez: In the next line +2 means: new char and null term */
                size_t newsize = row->raw_chars_size + padlen + 2;
                char *tmp = realloc(row->raw_chars, newsize);
                if (!tmp) return E_ERR_ALLOC;
                row->raw_chars = tmp;

                memset(row->raw_chars + row->raw_chars_size, ASCII_SPACE, padlen);
                row->raw_chars[row->raw_chars_size + padlen + 1] = '\0';
                row->raw_chars_size += padlen + 1;
        } else {
                /*
                 * Antirez: If we are in the middle of the string just make
                 * space for 1 new char plus the (already existing) null term.
                 */
                char *tmp = realloc(row->raw_chars, row->raw_chars_size + 2);
                if (!tmp) return E_ERR_ALLOC;
                row->raw_chars = tmp;

                memmove(row->raw_chars + offset + 1, row->raw_chars + offset,
                        row->raw_chars_size - offset + 1);
                row->raw_chars_size++;
        }

        row->raw_chars[offset] = c;
        e->unsaved_changes = true;

        return editor_row_render(row, &e->settings);
}

static inline void editor_rows_shift_back(struct editor *e, size_t row_pos)
{
        assert(e);

        memmove(e->rows + row_pos, e->rows + row_pos + 1,
                sizeof(e->rows[0]) * (e->row_cnt - row_pos - 1));
        for (size_t j = row_pos; j < e->row_cnt - 1; j++) e->rows[j]->g_pos--;
}

static inline void editor_row_unlink(struct editor_row *row)
{
        assert(row);

        if (row->prev) {
                row->prev->next = row->next;
                row->prev = NULL;
        }

        if (row->next) {
                row->next->prev = row->prev;
                row->next = NULL;
        }
}

static inline void editor_row_discard(struct editor_row *row)
{
        assert(row);

        freenull(row->rendered);
        row->rendered_size = 0;

        freenull(row->hl.type);
        freenull(row->raw_chars);
        row->raw_chars_size = 0;
}

static void editor_row_delete(struct editor *e, size_t row_pos)
{
        assert(e);
        assert(row_pos < e->row_cnt);

        struct editor_row *row = e->rows[row_pos];

        editor_row_unlink(row);
        editor_row_discard(row); freenull(row);
        editor_rows_shift_back(e, row_pos);

        e->row_cnt--;
        e->unsaved_changes = true;
}

/* ================================ Helpers ================================= */

static int editor_set_status_msg(
                struct editor_status *status, const char *fmt, ...)
{
        assert(status);
        assert(fmt);

        va_list ap;
        va_start(ap, fmt);
        status->msg_len = vsnprintf(status->msg, MAX_STATUS_MSG_LEN, fmt, ap);
        /*
         * If this assertion fails, I'll make a bigger buffer. In fact, the size
         * could potentially be dynamically determined by the monitor size and
         * the max number of characters the status message can take up on the
         * screen.
         */
        assert(status->msg_len < MAX_STATUS_MSG_LEN);
        va_end(ap);
        status->time = time(NULL);
        return (status->msg_len > 0 && status->msg_len < MAX_STATUS_MSG_LEN) ?
                0 : E_ERR_GENERAL;
}

/* ================================ File IO ================================= */

/*
 * Antirez: Turn the editor rows into a single heap-allocated string. Returns
 * the pointer to the heap-allocated string and populate the integer pointed by
 * 'buflen' with the size of the string, excluding the final nulterm.
 */
static char *editor_file_serialize(struct editor *e, size_t *outlen)
{
        assert(e);
        assert(outlen);

        size_t total_len = 0;
        for (size_t j = 0; j < e->row_cnt; j++) {
                /* Antirez: +1 is for "\n" at end of every row */
                total_len += e->rows[j]->raw_chars_size + 1;
        }

        *outlen = total_len;
        total_len++; /* Antirez: Also make space for nulterm. */

        char *buf = malloc(total_len);
        if (!buf) return NULL;

        char *p = buf;
        for (size_t j = 0; j < e->row_cnt; j++) {
                memcpy(p, e->rows[j]->raw_chars, e->rows[j]->raw_chars_size);
                p += e->rows[j]->raw_chars_size;
                *p = '\n';
                p++;
        }

        return buf;
}

static int editor_file_open(struct editor *e, const char *filename)
{
        assert(e);
        assert(filename);

        free(e->filename);
        size_t fnlen = strlen(filename) + 1;
        e->filename = malloc(fnlen);
        memcpy(e->filename, filename, fnlen);

        FILE *fp = fopen(filename, "r");
        if (!fp) {
                if (errno != ENOENT) {
                        perror("Opening file");
                        return E_ERR_FATAL;
                }
                return E_ERR_GENERAL;
        }

        int stat;
        char *line = NULL;
        size_t size = 0;
        ssize_t len;

        while ((len = getline(&line, &size, fp)) != -1) {
                if (line[len - 1] == '\n' || line[len - 1] == '\r') {
                        line[--len] = '\0';
                }
                if ((stat = editor_row_insert(e, line, (size_t)len, e->row_cnt))
                    != 0) {
                        break;
                }
        }

        free(line);
        fclose(fp);
        e->unsaved_changes = false;
        return stat;
}

static int editor_file_save(struct editor *e)
{
        assert(e);

        size_t len;
        char *buf = editor_file_serialize(e, &len);
        /*
         * To consider: Don't "crash" program if this particular function fails
         * (Any non-zero return will have the main loop exit.)
         */
        if (!buf) return E_ERR_ALLOC;

        int status = E_ERR_GENERAL;
        int fd = open(e->filename, O_WRONLY|O_CREAT, 0644);
        if (fd != -1) {
                if ((ftruncate(fd, len) != -1) &&
                    (write(fd, buf, len) == (ssize_t)len) && (fsync(fd) != -1)) {
                        e->unsaved_changes = false;
                        editor_set_status_msg(
                                &e->status, "%zu bytes written to disk", len);
                        status = 0;
                }
                close(fd);
        }
        free(buf);

        if (status != 0) {
                editor_set_status_msg(&e->status, "Can't save! I/O error: %s",
                                      strerror(errno));
        }
        return status;
}

/* ============================ Terminal update ============================= */

/* Must be a power of two larger than the largest escape code. */
#define APPEND_BUFFER_DEFAULT_SIZE 64

/*
 * Antirez: We define a very simple "append buffer" structure that is a heap
 * allocated string to which we can append. This is useful in order to write all
 * the escape sequences in a buffer and flush them to the standard output in a
 * single call, which helps avoid flickering effects.
 */
struct append_buffer {
        char *data;
        size_t len;
        size_t capacity;
};

struct escape_code {
        char *sequence;
        size_t len;
};

struct ansi_codes {
        struct escape_code
                        hide_cursor,
                        show_cursor,
                        go_home,
                        erase_to_eol, /* Erase to end of line */
                        swap_fg_bg,
                        default_fg,
                        reset_colors,

                        dsr, /* Device status report */
                        cursor_to_last_col,
                        cursor_to_last_row;
};

static const struct ansi_codes AnsiCodeDB = {
        .hide_cursor            = { "\x1b[?25l",6 },
        .show_cursor            = { "\x1b[?25h",6 },
        .go_home                = { "\x1b[H",   3 },
        .erase_to_eol           = { "\x1b[0K",  4 },
        .swap_fg_bg             = { "\x1b[7m",  4 },
        .default_fg             = { "\x1b[39m", 5 },
        .reset_colors           = { "\x1b[0m",  4 },

        .dsr                    = { "\x1b[6n",  4 },
        .cursor_to_last_col     = { "\x1b[999C",6 },
        .cursor_to_last_row     = { "\x1b[999B",6 },
};

static const char Newline[] = "\r\n";
static const size_t NewlineLen = sizeof(Newline) - 1;

/* -------------------------- Buffering mechanics --------------------------- */

static inline int build_ansi_code_cursor_pos(
                        char *buf, size_t buflen, int row, int col)
{
        int outlen = snprintf(buf, buflen, "\x1b[%d;%dH", row, col);
        return (buflen > (size_t)outlen && outlen > 0) ? outlen : E_ERR_GENERAL;
}

static inline int build_ansi_code_set_color(char *buf, size_t buflen, int color)
{
        int outlen = snprintf(buf, buflen, "\x1b[%dm", color);
        return (buflen > (size_t)outlen && outlen > 0) ? outlen : E_ERR_GENERAL;
}

static int append_buffer_init(struct append_buffer *ab)
{
        assert(ab);

        ab->capacity = APPEND_BUFFER_DEFAULT_SIZE;
        ab->data = malloc(ab->capacity);
        if (!ab->data) return E_ERR_ALLOC;
        ab->len = 0;
        return 0;
}

static int append_buffer(struct append_buffer *ab, const char *s, size_t len)
{
        assert(ab);
        assert(s);
        assert(len > 0);

        assert(ab->data && ab->capacity);

        if ((ab->len + len) >= ab->capacity) {
                ab->capacity *= 2;
                char *renewed = realloc(ab->data, ab->capacity);
                if (!renewed) {
                        return E_ERR_ALLOC;
                }
                ab->data = renewed;
        }

        memcpy(ab->data + ab->len, s, len);
        ab->len += len;
        return 0;
}

static void append_buffer_discard(struct append_buffer *ab)
{
        assert(ab);

        freenull(ab->data);
        ab->len = 0;
}

/* --------------------------- Handle empty rows ---------------------------- */

static int write_welcome_msg_to_ab(struct append_buffer *ab, int screencols)
{
        assert(ab);

        char welcome[80] = {0};
        int welcome_len = snprintf(welcome, sizeof(welcome),
                                   "Eram editor -- version %s", ERAM_VERSION);
        assert((size_t)welcome_len < sizeof(welcome) &&
               "The welcome message buffer is too small");

        int status;

        size_t padding = (screencols - welcome_len)/2;
        for (size_t i = 0; i < padding; i++) {
                char paddingchar = (i == 0) ? EmptyLineChar : ASCII_SPACE;
                if ((status = append_buffer(ab, &paddingchar, 1)) != 0) {
                        return status;
                }
        }

        if ((status = append_buffer(ab, welcome, welcome_len)) != 0) {
                return status;
        }

        if ((status = append_buffer(ab, AnsiCodeDB.erase_to_eol.sequence,
                                    AnsiCodeDB.erase_to_eol.len)) != 0) {
                return status;
        }

        return append_buffer(ab, Newline, NewlineLen);
}

static int handle_write_beyond_file(struct append_buffer *ab, int row_cnt,
                                    int screen_cols, int vertical_pos)
{
        assert(ab);

        if (row_cnt == 0 && vertical_pos == screen_cols/3) {
                return write_welcome_msg_to_ab(ab, screen_cols);
        }

        int status;

        if ((status = append_buffer(ab, &EmptyLineChar, 1)) != 0) {
                return status;
        }

        if ((status = append_buffer(ab, AnsiCodeDB.erase_to_eol.sequence,
                                    AnsiCodeDB.erase_to_eol.len)) != 0) {
                return status;
        }

        return append_buffer(ab, Newline, NewlineLen);
}

/* ------------------------ Buffer rows for display ------------------------- */

static int write_nonprint_to_buffer(struct append_buffer *ab, unsigned char c)
{
        assert(ab);

        int status;

        if ((status = append_buffer(ab, AnsiCodeDB.swap_fg_bg.sequence,
                                    AnsiCodeDB.swap_fg_bg.len)) != 0) {
                return status;
        }

        char sym = (c <= 26) ? '@' + c : '?';
        if ((status = append_buffer(ab, &sym, 1)) != 0) {
                return status;
        }

        return append_buffer(ab, AnsiCodeDB.reset_colors.sequence,
                             AnsiCodeDB.reset_colors.len);
}

static int write_normal_char_to_buffer(
                struct append_buffer *ab, char c, bool reset_fg)
{
        assert(ab);

        if (reset_fg) {
                int status = append_buffer(ab, AnsiCodeDB.default_fg.sequence,
                                           AnsiCodeDB.default_fg.len);
                if (status != 0) return status;
        }
        return append_buffer(ab, &c, 1);
}

static int write_highlighted_char_to_buffer(
                struct append_buffer *ab, char c, int color, int *current_color)
{
        assert(ab);
        assert(current_color);

        if (color != *current_color) {
                char seq[MAX_CODE_FMT_SIZE] = {0};
                int seq_len = build_ansi_code_set_color(seq, MAX_CODE_FMT_SIZE,
                                                        color);
                assert(seq_len != E_ERR_GENERAL &&
                       "The sequence buffer is too small");
                *current_color = color;
                int status = append_buffer(ab, seq, seq_len);
                if (status != 0) return status;
        }
        return append_buffer(ab, &c, 1);
}

static int buffer_screen_write(struct append_buffer *ab, struct editor_row *row,
                               int column_offset, int screen_cols,
                               struct editor_settings *settings)
{
        assert(ab);
        assert(row);
        assert(settings);
        assert(screen_cols > 0);

        int status;

        /* TO ANSWER: Can the column offset ever be negative? */
        size_t len = row->rendered_size - (size_t)column_offset;
        if (len > (size_t)screen_cols) {
                len = screen_cols;
        }
        int current_color = -1;

        char *c = row->rendered + column_offset;
        unsigned char *hl = row->hl.type + column_offset;

        for (size_t j = 0; j < len; j++) {
                switch (hl[j]) {
                case HL_TYPE_NONPRINT:
                        status = write_nonprint_to_buffer(ab, c[j]);
                        break;
                case HL_TYPE_NORMAL:
                        status = write_normal_char_to_buffer(
                                        ab, c[j], (current_color != -1));
                        current_color = -1;
                        break;
                default:
                {
                        int color = get_hl_type_color(hl[j], settings);
                        status = write_highlighted_char_to_buffer(
                                        ab, c[j], color, &current_color);
                        break;
                }
                }
                if (status != 0) return status;
        }

        if ((status = append_buffer(ab, AnsiCodeDB.default_fg.sequence,
                                    AnsiCodeDB.default_fg.len)) != 0) {
                return status;
        }

        if ((status = append_buffer(ab, AnsiCodeDB.erase_to_eol.sequence,
                                    AnsiCodeDB.erase_to_eol.len)) != 0) {
                return status;
        }

        return append_buffer(ab, Newline, NewlineLen);
}

/* ---------------------------- Write status bar ---------------------------- */

static int write_status_bar1(struct append_buffer *ab, const struct editor *e)
{
        assert(ab);
        assert(e);

        int ab_stat;

        if ((ab_stat = append_buffer(ab, AnsiCodeDB.erase_to_eol.sequence,
                                     AnsiCodeDB.erase_to_eol.len)) != 0) {
                return ab_stat;
        }

        if ((ab_stat = append_buffer(ab, AnsiCodeDB.swap_fg_bg.sequence,
                                     AnsiCodeDB.swap_fg_bg.len)) != 0) {
                return ab_stat;
        }

        char status[MAX_CODE_FMT_SIZE] = {0};
        int status_len = snprintf(status, sizeof(status), "%.20s - %ld lines %s",
                                  e->filename, e->row_cnt, e->unsaved_changes ?
                                  MODIFICATION_NOTICE_MSG : "");
        assert(status_len < MAX_CODE_FMT_SIZE &&
               "Status bar buffer is too small");

        char rstatus[MAX_CODE_FMT_SIZE] = {0};
        int rstatus_len = snprintf(rstatus, sizeof(rstatus), "%d,%ld",
                                   e->row_offset + e->cy + 1, e->row_cnt);
        assert(rstatus_len < MAX_CODE_FMT_SIZE &&
               "Right hand status bar buffer is too small");

        if (status_len > e->num_screen_cols) {
                status_len = e->num_screen_cols;
        }

        if ((ab_stat = append_buffer(ab, status, status_len)) != 0) {
                return ab_stat;
        }

        int pos = status_len;
        while (pos < e->num_screen_cols) {
                if (e->num_screen_cols - pos == rstatus_len) {
                        if ((ab_stat = append_buffer(ab, rstatus, rstatus_len))
                            != 0) {
                                return ab_stat;
                        }
                        break;
                } else {
                        char space = ASCII_SPACE;
                        if ((ab_stat = append_buffer(ab, &space, 1)) != 0) {
                                return ab_stat;
                        }
                        pos++;
                }
        }

        if ((ab_stat = append_buffer(ab, AnsiCodeDB.reset_colors.sequence,
                                     AnsiCodeDB.reset_colors.len)) != 0) {
                return ab_stat;
        }

        return append_buffer(ab, Newline, NewlineLen);
}

/*
 * Antirez: The second row depends on the editor's status message
 * and status update time.
 */
static int write_status_bar2(struct append_buffer *ab, const struct editor *e)
{
        assert(ab);
        assert(e);

        int ab_stat;

        if ((ab_stat = append_buffer(ab, AnsiCodeDB.erase_to_eol.sequence,
                                     AnsiCodeDB.erase_to_eol.len)) != 0) {
                return ab_stat;
        }

        if (e->status.msg_len > 0 && (time(NULL) - e->status.time <
            e->status.msg_timeout)) {
                size_t outlen = (e->status.msg_len < e->num_screen_cols) ?
                                 e->status.msg_len : e->num_screen_cols;
                if ((ab_stat = append_buffer(ab, e->status.msg, outlen)) != 0) {
                        return ab_stat;
                }
        }

        /*
         * Antirez: Put cursor at its current position. Note that the horizontal
         * position at which the cursor is displayed may be different compared
         * to 'E.cx' because of TABs.
         */
        int cx = 1;
        struct editor_settings settings = e->settings;
        int filerow = e->row_offset + e->cy;
        struct editor_row *row =
                ((size_t)filerow >= e->row_cnt) ? NULL : e->rows[filerow];

        if (row) {
                int j = e->column_offset;
                for (; j < (e->cx + e->column_offset); j++) {
                        if ((size_t)j < row->raw_chars_size &&
                            row->raw_chars[j] == TAB) {
                                cx += (settings.tab_size - 1) -
                                      (cx % settings.tab_size);
                        }
                        cx++;
                }
        }

        char buf[MAX_CODE_FMT_SIZE] = {0};
        int buflen = build_ansi_code_cursor_pos(buf, MAX_CODE_FMT_SIZE,
                                                e->cy + 1, cx);
        assert(buflen != E_ERR_GENERAL &&
               "The format code buffer is too small");
        return append_buffer(ab, buf, buflen);
}

static int write_status_bar_to_buffer(
                        struct append_buffer *ab, const struct editor *e)
{
        assert(ab);
        assert(e);

        int ab_stat = write_status_bar1(ab, e);
        if (ab_stat != 0) {
                return ab_stat;
        }
        return write_status_bar2(ab, e);
}

/* ----------------------------- Handle refresh ----------------------------- */

/*
 * Antirez: This function uses the whole screen using VT100 escape characters
 * starting from the logical state of the editor.
 */
static int editor_refresh_screen(struct editor *e)
{
        assert(e);

        int status;
        struct append_buffer ab;

        if ((status = append_buffer_init(&ab)) != 0) {
                return status;
        }

        if ((status = append_buffer(&ab, AnsiCodeDB.hide_cursor.sequence,
                                    AnsiCodeDB.hide_cursor.len)) != 0) {
                return status;
        }

        if ((status = append_buffer(&ab, AnsiCodeDB.go_home.sequence,
                                    AnsiCodeDB.go_home.len)) != 0) {
                return status;
        }

        for (int y = 0; y < e->num_screen_rows; y++) {
                int filerow = e->row_offset + y;

                /*
                 * TO ANSWER: What needs to be an integer and what can be a
                 * size_t? I feel like integers were used out of habit in kilo,
                 * not for any particular reason -- but I don't want to just
                 * remove them without being positive as to why they were used
                 * in the first place.
                 */
                if ((size_t)filerow >= e->row_cnt) {
                        status = handle_write_beyond_file(
                                        &ab, e->row_cnt, e->num_screen_cols, y);
                } else {
                        status = buffer_screen_write(
                                        &ab, e->rows[filerow], e->column_offset,
                                        e->num_screen_cols, &e->settings);
                }
                if (status != 0) return status;
        }

        if ((status = write_status_bar_to_buffer(&ab, e)) != 0) {
                return status;
        }

        if ((status = append_buffer(&ab, AnsiCodeDB.show_cursor.sequence,
                                    AnsiCodeDB.show_cursor.len)) != 0) {
                return status;
        }

        ssize_t outval = write(e->out_fd, ab.data, ab.len);
        if (outval == -1 || (size_t)outval != ab.len) {
                status = E_ERR_GENERAL;
        }

        append_buffer_discard(&ab);
        return status;
}

static int editor_clear_screen(struct editor *e)
{
        assert(e);

        int status;
        struct append_buffer ab;

        if ((status = append_buffer_init(&ab)) != 0) {
                return status;
        }

        if ((status = append_buffer(&ab, AnsiCodeDB.hide_cursor.sequence,
                                    AnsiCodeDB.hide_cursor.len)) != 0) {
                return status;
        }

        if ((status = append_buffer(&ab, AnsiCodeDB.go_home.sequence,
                                    AnsiCodeDB.go_home.len)) != 0) {
                return status;
        }

        if ((status = append_buffer(&ab, AnsiCodeDB.show_cursor.sequence,
                                    AnsiCodeDB.show_cursor.len)) != 0) {
                return status;
        }

        ssize_t outval = write(e->out_fd, ab.data, ab.len);
        if (outval == -1 || (size_t)outval != ab.len) {
                status = E_ERR_GENERAL;
        }

        append_buffer_discard(&ab);
        return status;
}

/* ============================= Event handling ============================= */

/* ------------------------------- Find mode -------------------------------- */

#define ERAM_QUERY_LEN 256

enum editor_find_status {
        E_FIND_CONTINUE,
        E_FIND_EXIT,
};

enum editor_find_direction {
        FIND_DIRECTION_BACK     = -1,
        FIND_DIRECTION_NONE     =  0,
        FIND_DIRECTION_FWD      =  1,
};

struct editor_find_ctx {
        char query[ERAM_QUERY_LEN + 1];
        size_t qlen;

        bool match_found;
        size_t match_offset;
        size_t last_match_ln;
        enum editor_find_direction dir;
        size_t saved_hl_ln;
        unsigned char *saved_hl;
};

struct editor_orig_state {
        const int saved_cx;
        const int saved_cy;
        const int saved_coloff;
        const int saved_rowoff;
};

static int editor_get_char_from_input(int in_fd);

static void editor_find_restore_hl(struct editor *e, struct editor_find_ctx *f)
{
        assert(e);
        assert(f);

        if (f->saved_hl && (f->saved_hl_ln < e->row_cnt)) {
                memcpy(e->rows[f->saved_hl_ln]->hl.type, f->saved_hl,
                       e->rows[f->saved_hl_ln]->rendered_size);
                freenull(f->saved_hl);
        }
}

static int editor_find_ctx_init(struct editor_find_ctx *ctx)
{
        assert(ctx);

        ctx->dir = FIND_DIRECTION_NONE;
        ctx->match_found = false;
        ctx->match_offset = 0;
        ctx->last_match_ln = 0;
        ctx->saved_hl = NULL;
        ctx->saved_hl_ln = 0;
        ctx->qlen = 0;
        memset(ctx->query, '\0', sizeof(ctx->query));

        return 0;
}

static void editor_find_ctx_discard(struct editor_find_ctx *ctx)
{
        assert(ctx);
        freenull(ctx->saved_hl);
}

static struct editor_orig_state editor_find_save_state(struct editor *e)
{
        assert(e);
        return (struct editor_orig_state) {
                .saved_cx = e->cx,
                .saved_cy = e->cy,
                .saved_rowoff = e->row_offset,
                .saved_coloff = e->column_offset,
        };
}

static void editor_find_restore_state(
                struct editor *e, const struct editor_orig_state *orig_st)
{
        assert(e);
        assert(orig_st);

        e->cx = orig_st->saved_cx;
        e->cy = orig_st->saved_cy;
        e->row_offset = orig_st->saved_rowoff;
        e->column_offset = orig_st->saved_coloff;
}

static enum editor_find_status editor_find_handle_key(
                                struct editor *e, struct editor_find_ctx *f,
                                int c, const struct editor_orig_state *orig_st)
{
        assert(e);
        assert(f);
        assert(orig_st);

        if (c == DEL_KEY || c == CTRL_H || c == BACKSPACE) {
                if (f->qlen != 0) {
                        f->query[--f->qlen] = '\0';
                }
                f->match_found = false;
        } else if (c == ESC || c == ENTER) {
                if (c == ESC) {
                        editor_find_restore_state(e, orig_st);
                }
                editor_find_restore_hl(e, f);
                editor_set_status_msg(&e->status, "");
                return E_FIND_EXIT;
        } else if (c == ARROW_RIGHT || c == ARROW_DOWN) {
                f->dir = FIND_DIRECTION_FWD;
        } else if (c == ARROW_LEFT || c == ARROW_UP) {
                f->dir = FIND_DIRECTION_BACK;
        } else if (isprint(c)) {
                if (f->qlen < ERAM_QUERY_LEN) {
                        f->query[f->qlen++] = c;
                        f->query[f->qlen] = '\0';
                        f->match_found = false;
                }
        }
        return E_FIND_CONTINUE;
}

static int editor_find_jump_to_match(
                struct editor *e, const struct editor_find_ctx *f)
{
        assert(e);
        assert(f);
        assert(f->match_found);

        if (!f->match_found) return E_ERR_GENERAL;

        e->cy = 0;
        e->cx = f->match_offset;
        e->row_offset = f->last_match_ln;
        e->column_offset = 0;

        /* Scroll back horizontally as needed */
        if (e->cx > e->num_screen_cols) {
                int diff = e->cx - e->num_screen_cols;
                e->cx -= diff;
                e->column_offset += diff;
        }
        return 0;
}

static int editor_find_render_match(struct editor *e, struct editor_find_ctx *f)
{
        assert(e);
        assert(f);
        assert(f->last_match_ln < e->row_cnt);
        assert(f->match_found);

        if (!f->match_found) {
                return E_ERR_GENERAL;
        }

        editor_find_restore_hl(e, f);
        struct editor_row *row = e->rows[f->last_match_ln];
        if (row->hl.type) {
                f->saved_hl_ln = f->last_match_ln;
                f->saved_hl = malloc(row->rendered_size);
                if (!f->saved_hl) {
                        return E_ERR_ALLOC;
                }
                memcpy(f->saved_hl, row->hl.type, row->rendered_size);
                memset(row->hl.type + f->match_offset, HL_TYPE_MATCH, f->qlen);
        }

        return 0;
}

static bool editor_find_has_match(struct editor *e, struct editor_find_ctx *f)
{
        assert(e);
        assert(f);

        if (!f->match_found) f->dir = FIND_DIRECTION_FWD;
        if (f->dir == FIND_DIRECTION_NONE) return false;

        char *match = NULL;
        size_t current = f->last_match_ln;

        size_t i = 0;
        for (; i < e->row_cnt; i++) {
                if (f->dir == FIND_DIRECTION_FWD) {
                        current = (current + 1) % e->row_cnt;
                } else {
                        current = (current == 0) ? e->row_cnt - 1 : current - 1;
                }
                struct editor_row *row = e->rows[current];
                match = strstr(row->rendered, f->query);
                if (match) {
                        f->last_match_ln = current;
                        f->match_found = true;
                        f->match_offset = match - row->rendered;
                        break;
                }
        }
        if (i == e->row_cnt) f->match_found = false;
        f->dir = FIND_DIRECTION_NONE;

        return f->match_found;
}

static int editor_find(struct editor *e)
{
        assert(e);

        struct editor_find_ctx f;
        editor_find_ctx_init(&f);
        struct editor_orig_state orig_st = editor_find_save_state(e);

        int status;
        for (;;) {
                editor_set_status_msg(&e->status,
                        "Search: %s (Use ESC/Arrows/Enter)", f.query);
                if ((status = editor_refresh_screen(e)) != 0) break;

                int c = editor_get_char_from_input(e->in_fd);
                enum editor_find_status find_status =
                        editor_find_handle_key(e, &f, c, &orig_st);
                if (find_status != E_FIND_CONTINUE) break;

                if (editor_find_has_match(e, &f)) {
                        status = editor_find_render_match(e, &f);
                        if (status != 0) break;
                        editor_find_jump_to_match(e, &f);
                }
        }

        editor_find_ctx_discard(&f);
        return status;
}

/* --------------------------- Key press handlers --------------------------- */

/*
 * Antirez: Read a key from the terminal put in raw mode, trying to handle
 * escape sequences.
 */
static int editor_get_char_from_input(int in_fd)
{
        int nread;
        char c, seq[3];

        while ((nread = read(in_fd, &c, 1)) == 0);
        if (nread == -1) return E_ERR_FATAL;

        if (c != ESC) return c;

        /* Antirez: If this is just an ESC, we'll timeout here. */
        if (read(in_fd, seq, 1) == 0) return ESC;
        if (read(in_fd, seq + 1, 1) == 0) return ESC;

        /* ESC [ sequences */
        if (seq[0] == '[') {
                if (seq[1] >= '0' && seq[1] <= '9') {
                        /* Extended escape sequence, read additional byte. */
                        if (read(in_fd, seq + 2, 1) == 0) {
                                return ESC;
                        }
                        if (seq[2] == '~') {
                                switch (seq[1]) {
                                case '3': return DEL_KEY;
                                case '5': return PAGE_UP;
                                case '6': return PAGE_DOWN;
                                default: goto err_out;
                                }
                        }
                } else {
                        switch (seq[1]) {
                        case 'A': return ARROW_UP;
                        case 'B': return ARROW_DOWN;
                        case 'C': return ARROW_RIGHT;
                        case 'D': return ARROW_LEFT;
                        case 'H': return HOME_KEY;
                        case 'F': return END_KEY;
                        default: goto err_out;
                        }
                }
        }

err_out:
        return c;
}

static int editor_handle_newline(struct editor *e)
{
        assert(e);

        int status = E_ERR_GENERAL;

        size_t filerow = e->row_offset + e->cy;
        size_t filecol = e->column_offset + e->cx;

        struct editor_row *row = (filerow >= e->row_cnt) ? NULL :
                                                           e->rows[filerow];
        if (!row) {
                if (filerow == e->row_cnt) {
                        status = editor_row_insert(e, "", 0, filerow);
                        goto fix_cursor;
                }
                /*
                 * TO ANSWER: What return type should this be? I feel like we
                 * should never reach this branch, but I'm not sure.
                 */
                return 0;
        }

        /*
         * Antirez: If the cursor is over the current line size, we want to
         * conceptually think about it like it's just over the last character.
         */
        if (filecol >= row->raw_chars_size) filecol = row->raw_chars_size;

        if (filecol == 0) {
                status = editor_row_insert(e, "", 0, filerow);
        } else {
                /*
                 * Antirez: We are in the middle of a line. Split between two
                 * rows.
                 */
                /*
                 * TODO: Track down the side effects of this function to
                 * determine why row has to be re-assigned after.
                 */
                status = editor_row_insert(
                                e, row->raw_chars + filecol,
                                row->raw_chars_size - filecol, filerow + 1);
                if (status != 0) goto fix_cursor;

                assert(row == e->rows[filerow] &&
                       "I just wanna know if this reassignment is necessary");
                row = e->rows[filerow];
                row->raw_chars[filecol] = '\0';
                row->raw_chars_size = filecol;
                status = editor_row_render(row, &e->settings);
        }

fix_cursor:
        if (e->cy == e->num_screen_rows - 1) {
                e->row_offset++;
        } else {
                e->cy++;
        }
        e->cx = 0;
        e->column_offset = 0;
        return status;
}

static inline int editor_move_cursor_left(struct editor *e, int filerow)
{
        assert(e);

        if (e->cx != 0) {
                e->cx--;
                return 0;
        }

        /* I _think_ this is what he meant. */
        if (e->column_offset > 0) {
                e->column_offset--;
        } else if (filerow > 0) {
                e->cy--;
                e->cx = e->rows[filerow - 1]->raw_chars_size;

                if (e->cx > e->num_screen_cols - 1) {
                        e->column_offset =
                                e->cx - e->num_screen_cols + 1;
                        e->cx = e->num_screen_cols - 1;
                }
        }
        return 0;
}

static inline int editor_move_cursor_right(
                        struct editor *e, struct editor_row *row, int filecol)
{
        assert(e);

        if (row && (size_t)filecol < row->raw_chars_size) {
                if (e->cx == e->num_screen_cols - 1) {
                        e->column_offset--;
                } else {
                        e->cx++;
                }
        } else if (row && (size_t)filecol == row->raw_chars_size) {
                e->cx = 0;
                e->column_offset = 0;
                if (e->cy == e->num_screen_rows - 1) {
                        e->row_offset++;
                } else {
                        e->cy++;
                }
        }
        return 0;
}

static inline int editor_move_cursor_up(struct editor *e)
{
        assert(e);

        if (e->cy == 0) {
                if (e->row_offset) {
                        e->row_offset--;
                }
        } else {
                e->cy--;
        }
        return 0;
}

static inline int editor_move_cursor_down(struct editor *e, int filerow)
{
        assert(e);

        if ((size_t)filerow < e->row_cnt) {
                if (e->cy == e->num_screen_rows - 1) {
                        e->row_offset++;
                } else {
                        e->cy++;
                }
        }
        return 0;
}

/* Antirez: Handle cursor position change because arrow keys were pressed. */
static int editor_handle_arrow_key(struct editor *e, int key)
{
        assert(e);

        int status = E_ERR_GENERAL;

        size_t filerow = e->row_offset + e->cy;
        size_t filecol = e->column_offset + e->cx;

        struct editor_row *row = (filerow >= e->row_cnt) ? NULL :
                                                           e->rows[filerow];

        switch (key) {
        case ARROW_LEFT:
                status = editor_move_cursor_left(e, filerow);
                break;
        case ARROW_RIGHT:
                status = editor_move_cursor_right(e, row, filecol);
                break;
        case ARROW_UP:
                status = editor_move_cursor_up(e);
                break;
        case ARROW_DOWN:
                status = editor_move_cursor_down(e, filerow);
                break;
        default:
                assert(false && "Bad input");
                /* TO CONSIDER: This might be an "EINVAL" style error */
                return E_ERR_GENERAL;
        }

        /* Antirez: Fix cx if the current line has not enough chars. */
        filerow = e->row_offset + e->cy;
        filecol = e->column_offset + e->cx;

        row = (filerow >= e->row_cnt) ? NULL : e->rows[filerow];
        size_t rowlen = row ? row->raw_chars_size : 0;

        if (filecol > rowlen) {
                e->cx -= filecol - rowlen;
                if (e->cx < 0) {
                        e->column_offset += e->cx;
                        e->cx = 0;
                }
        }
        return status;
}

static int editor_handle_page_up_down(struct editor *e, int key)
{
        assert(e);
        assert((key == PAGE_UP || key == PAGE_DOWN) && "Bad input");

        if ((key == PAGE_UP) && (e->cy != 0)) {
                e->cy = 0;
        } else if ((key == PAGE_DOWN) && (e->cy != (e->num_screen_rows - 1))) {
                e->cy = e->num_screen_rows - 1;
        }

        int status;
        int times = e->num_screen_rows;
        int c = key == PAGE_UP ? ARROW_UP : ARROW_DOWN;

        while (times--) {
                if ((status = editor_handle_arrow_key(e, c)) != 0) {
                        return status;
                }
        }
        return status;
}

static int editor_handle_delete_char(struct editor *e)
{
        assert(e);

        size_t filerow = e->row_offset + e->cy;
        int filecol = e->column_offset + e->cx;
        if ((filerow >= e->row_cnt) || (filerow == 0 && filecol == 0)) return 0;

        int status;
        if (filecol == 0) {
                filecol = e->rows[filerow - 1]->raw_chars_size;

                status = editor_row_append(
                                e, filerow, e->rows[filerow]->raw_chars,
                                e->rows[filerow]->raw_chars_size);
                if (status != 0) return status;
                editor_row_delete(e, filerow);

                if (e->cy == 0) {
                        e->row_offset--;
                } else {
                        e->cy--;
                }

                e->cx = filecol;
                if (e->cx >= e->num_screen_cols) {
                        int shift = (e->num_screen_cols - e->cx) + 1;
                        e->cx -= shift;
                        e->column_offset -= shift;
                }
        } else {
                status = editor_row_delete_char(e, filerow, filecol - 1);
                if (status != 0) return status;

                if (e->cx == 0 && e->column_offset) {
                        e->column_offset--;
                } else {
                        e->cx--;
                }
        }

        e->unsaved_changes = true;
        return status;
}

static int editor_handle_char_insert(struct editor *e, int c)
{
        assert(e);

        size_t filerow = e->row_offset + e->cy;
        size_t filecol = e->column_offset + e->cx;

        int status;
        /*
         * Antirez: If the row where the cursor is currently located does not
         * exist in our logical representation of the file, add enough empty
         * rows as needed.
         */
        if (filerow >= e->row_cnt) {
                while (filerow >= e->row_cnt) {
                        status = editor_row_insert(e, "", 0, e->row_cnt);
                        if (status != 0) return status;
                }
        }

        if ((status = editor_row_insert_char(e, filerow, filecol, c)) != 0) {
                return status;
        }

        if (e->cx == e->num_screen_cols - 1) {
                e->column_offset++;
        } else {
                e->cx++;
        }

        e->unsaved_changes = true;
        return status;
}

/* ------------------------- Main key press handler ------------------------- */

static int editor_process_key_press(struct editor *e)
{
        assert(e);

        int status = 0; /* Default to OK */
        static bool quit_requested = false;

        int c = editor_get_char_from_input(e->in_fd);
        switch (c) {
        case ENTER:
                status = editor_handle_newline(e);
                break;
        case CTRL_C:
                /*
                 * Antirez: We ignore ctrl-c, it can't be so simple to lose the
                 * changes to the edited file.
                 */
                break;
        case CTRL_Q:
                if (e->unsaved_changes && !quit_requested) {
                        editor_set_status_msg(&e->status, "WARNING: File has "
                                "unsaved changes. Press Ctrl-S to save. Press "
                                "Ctrl-Q again to quit.");
                        quit_requested = true;
                } else {
                        UserExit = 1;
                }
                return 0;
        case CTRL_S:
                status = editor_file_save(e);
                break;
        case CTRL_F:
                status = editor_find(e);
                break;
        case BACKSPACE:
        case CTRL_H:
        case DEL_KEY:
                status = editor_handle_delete_char(e);
                break;
        case PAGE_UP:
        case PAGE_DOWN:
                status = editor_handle_page_up_down(e, c);
                break;
        case ARROW_UP:
        case ARROW_DOWN:
        case ARROW_LEFT:
        case ARROW_RIGHT:
                status = editor_handle_arrow_key(e, c);
                break;
        case CTRL_L:
                /* Antirez Just refresh the line as a side effect. */
                /*
                 * This comment makes it seem like something needs to be done
                 * here. Does it? The easiest way to find out might be during
                 * functional testing.
                 */
                break;
        case ESC:
                /* Antirez: Nothing to do for ESC in this mode. */
                break;
        default:
                status = editor_handle_char_insert(e, c);
                break;
        }

        quit_requested ^= quit_requested;
        return status;
}

/* ====================== Handle window resizing event ====================== */

static void handle_window_resize_signal(int unused __attribute__((unused)))
{
        WindowSizeChanged = 1;
}


/*
 * Antirez: Use the ESC [6n escape sequence to query the horizontal cursor
 * position and return it. On error -1 is returned, on success the position of
 * the cursor is stored at *rows and *cols and 0 is returned.
 */
/*
 * The types and sizes of the rows and columns variables needs to be determined
 * by the maximum screen size supported by the editor.
 */
static int get_cursor_pos(int in_fd, int out_fd, int *rows, int *cols)
{
        assert(rows);
        assert(cols);

        /* Report cursor location */
        if (write(out_fd, AnsiCodeDB.dsr.sequence, AnsiCodeDB.dsr.len)
            != (ssize_t)AnsiCodeDB.dsr.len) {
                return E_ERR_GENERAL;
        }

        size_t i = 0;
        char buf[32] = {0};
        for (; i < sizeof(buf) - 1; i++) {
                /* Break on failed read or end of device status report */
                if (read(in_fd, buf + i, 1) != 1 || buf[i] == 'R') {
                        break;
                }
        }
        buf[i] = '\0';

        /*
         * The DSR return sequence is "ESC[n;mR", where 'n' is the number of
         * rows and 'm' is the number of columns.
         */
        if (buf[0] != ESC || buf[1] != '[') return E_ERR_GENERAL;
        if (sscanf(buf + 2, "%d;%d", rows, cols) != 2) return E_ERR_GENERAL;

        return 0;
}

/*
 * Antirez: Try to get the number of columns in the current terminal. If the
 * ioctl() call fails the function will try to query the terminal itself.
 * Returns 0 on success, -1 on error.
 */
/*
 * I use enum codes for failure, and I have one for FATAL, meaning that the
 * caller should be able to propagate that code upward and terminate the program.
 */
static int get_window_size(struct editor *e)
{
        assert(e);

        struct winsize ws;
        if (ioctl(e->out_fd, TIOCGWINSZ, &ws) == 0 && ws.ws_col > 0) {
                e->num_screen_cols = ws.ws_col;
                e->num_screen_rows = ws.ws_row;
                return 0;
        }

        int orig_row = 0, orig_col = 0;
        if (get_cursor_pos(e->in_fd, e->out_fd, &orig_row, &orig_col) != 0) {
                return E_ERR_GENERAL;
        }

        if (write(e->out_fd, AnsiCodeDB.cursor_to_last_col.sequence,
                  AnsiCodeDB.cursor_to_last_col.len) !=
            (ssize_t)AnsiCodeDB.cursor_to_last_col.len) {
                return E_ERR_GENERAL;
        }

        if (write(e->out_fd, AnsiCodeDB.cursor_to_last_row.sequence,
                  AnsiCodeDB.cursor_to_last_row.len) !=
            (ssize_t)AnsiCodeDB.cursor_to_last_row.len) {
                return E_ERR_GENERAL;
        }

        if (get_cursor_pos(e->in_fd, e->out_fd, &e->num_screen_rows,
                           &e->num_screen_cols) != 0) {
                return E_ERR_GENERAL;
        }

        char seq[MAX_CODE_FMT_SIZE] = {0};
        int seq_len = build_ansi_code_cursor_pos(seq, MAX_CODE_FMT_SIZE,
                                                 orig_row, orig_col);
        assert(seq_len != E_ERR_GENERAL && "The sequence buffer is too small");

        if (write(e->out_fd, seq, seq_len) == -1) {
                return E_ERR_FATAL;
        }

        return 0;
}

static int update_window_size(struct editor *e)
{
        assert(e);

        if (get_window_size(e) != 0) {
                perror("Unable to query the screen for size");
                return E_ERR_FATAL;
        }
        e->num_screen_rows -= STATUS_BAR_HEIGHT;
        WindowSizeChanged = 0;
        return 0;
}

static int handle_window_resize_change(struct editor *e)
{
        assert(e);

        int stat = update_window_size(e);
        if (stat != 0) return E_ERR_FATAL;

        if (e->cy > e->num_screen_rows) {
                e->cy = e->num_screen_rows - 1;
       }

        if (e->cx > e->num_screen_cols) {
                e->cx = e->num_screen_cols - 1;
        }

        return 0;
}

/* ============================= Initialization ============================= */

static struct editor_syntax *select_syntax_highlighting(char *filename)
{
        assert(filename);

        for (size_t i = 0; i < HIGHLIGHT_DB_ENTRY_CNT; i++) {
                struct editor_syntax *syntax = HighlightDB + i;
                size_t j = 0;
                while (syntax->filematch[j]) {
                        size_t pattern_len = strlen(syntax->filematch[j]);
                        char *pattern = strstr(filename, syntax->filematch[j]);
                        if (pattern && (syntax->filematch[j][0] != '.' ||
                                        pattern[pattern_len] == '\0')) {
                                return syntax;
                        }
                        j++;
                }
        }
        return NULL; /* No matches found */
}

static int editor_settings_init(struct editor_settings *settings,
                                char *filename)
{
        static_assert(HL_TYPE_OUT_OF_RANGE <= UCHAR_MAX);

        assert(settings);

        settings->tab_size = 8;
        settings->syntax = select_syntax_highlighting(filename);
        if (!settings->syntax) return 0;

        struct editor_token_group *tokens = &settings->syntax->tokens;

        tokens->hl_type[TOKEN_SL_COMMENT_START] = HL_TYPE_COMMENT;
        tokens->hl_type[TOKEN_ML_COMMENT_START] = HL_TYPE_ML_COMMENT;
        tokens->hl_type[TOKEN_ML_COMMENT_END]   = HL_TYPE_ML_COMMENT;

        /* Do not allow configurations to touch these settings */
        tokens->token_boundary[TOKEN_SL_COMMENT_START] = HL_BOUNDARY_START;
        tokens->token_boundary[TOKEN_ML_COMMENT_START] = HL_BOUNDARY_START;
        tokens->token_boundary[TOKEN_ML_COMMENT_END]   = HL_BOUNDARY_END;

        /* Will be configurable in the future */
        settings->syntax->colors.comment    = HL_COLOR_COMMENT_DEFAULT;
        settings->syntax->colors.keyword1   = HL_COLOR_KEYWORD1_DEFAULT;
        settings->syntax->colors.keyword2   = HL_COLOR_KEYWORD2_DEFAULT;
        settings->syntax->colors.match      = HL_COLOR_MATCH_DEFAULT;
        settings->syntax->colors.ml_comment = HL_COLOR_ML_COMMENT_DEFAULT;
        settings->syntax->colors.normal     = HL_COLOR_NORMAL_DEFAULT;
        settings->syntax->colors.number     = HL_COLOR_NUMBER_DEFAULT;
        settings->syntax->colors.string     = HL_COLOR_STRING_DEFAULT;

        return 0;
}

static void editor_discard(struct editor *e)
{
        assert(e);

#if DEBUG
        if (LogFD != STDOUT_FILENO && LogFD != STDERR_FILENO) {
                close(LogFD);
        }
#endif

        for (size_t i = 0; i < e->row_cnt; i++) {
                editor_row_delete(e, i);
        }
}

static int editor_init(struct editor *e, char *filename)
{
        assert(e);
        assert(filename);

#if DEBUG
        LogFD = open("log.txt", O_WRONLY|O_CREAT|O_TRUNC, 0644);

        time_t t = time(NULL);
        dprintf(LogFD, "%s\n", ctime(&t));
#endif

        e->cx = 0;
        e->cy = 0;
        e->row_offset = 0;
        e->column_offset = 0;
        e->num_screen_rows = 0;
        e->row_cnt = 0;
        e->rows_cap = 0;
        e->rows = NULL;
        e->unsaved_changes = false;
        e->filename = NULL;
        e->in_raw_mode = false;
        e->in_fd = STDIN_FILENO;
        e->out_fd = STDOUT_FILENO;

        e->status.msg_len = 0;
        e->status.msg_timeout = STATUS_MSG_TIMEOUT;

        if (editor_settings_init(&e->settings, filename) != 0) {
                return E_ERR_GENERAL;
        }

        int status = update_window_size(e);
        if (status == E_ERR_FATAL) return status;
        signal(SIGWINCH, handle_window_resize_signal);

        return 0;
}

static void editor_print_err_code_msg(enum editor_error err)
{
        switch (err) {
        case E_ERR_ALLOC:
                fprintf(stderr, "eram: Out of memory\n");
                break;
        case E_ERR_FATAL:
                fprintf(stderr, "eram: Fatal: cannot continue\n");
                break;
        case E_ERR_GENERAL:
                fprintf(stderr, "eram: Something went wrong\n");
                break;
        default:
                break;
        }
}

int main(int argc, char **argv)
{
        if (argc != 2) {
                fprintf(stderr, "Usage: eram <filename>\n");
                exit(EXIT_FAILURE);
        }

        int stat;
        struct editor e;
        char *filename = argv[1];

        if ((stat = editor_init(&e, filename)) != 0) exit(stat);

        editor_file_open(&e, filename);
        editor_enable_raw_mode(&e);
        editor_set_status_msg(&e.status,
                "Help: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

        while (!UserExit) {
                if (WindowSizeChanged &&
                    ((stat = handle_window_resize_change(&e)) != 0)) break;

                if ((stat = editor_refresh_screen(&e)) != 0) break;
                if ((stat = editor_process_key_press(&e)) != 0) break;
        }

        editor_clear_screen(&e);
        editor_disable_raw_mode(&e);
        editor_discard(&e);
        editor_print_err_code_msg(stat);

        return stat;
}
