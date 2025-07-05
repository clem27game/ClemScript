#define _GNU_SOURCE  // For strdup and strndup
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>   // For srand, rand, nanosleep (or usleep)
#include <math.h>   // For mathematical functions
#ifdef _WIN32
#include <windows.h> // For Sleep on Windows
#else
#include <unistd.h>  // For usleep on POSIX systems
#include <strings.h> // For strcasecmp on POSIX systems
#endif

// --- Constants and Enums ---

// ANSI Escape Codes for Colors
#define ANSI_COLOR_RED     "\x1b[31m"
#define ANSI_COLOR_GREEN   "\x1b[32m"
#define ANSI_COLOR_YELLOW  "\x1b[33m"
#define ANSI_COLOR_BLUE    "\x1b[34m"
#define ANSI_COLOR_MAGENTA "\x1b[35m"
#define ANSI_COLOR_CYAN    "\x1b[36m"
#define ANSI_COLOR_WHITE   "\x1b[37m"
#define ANSI_COLOR_RESET   "\x1b[0m"

// Token Types
typedef enum {
    TOKEN_EOF,
    TOKEN_IDENTIFIER,
    TOKEN_INT,
    TOKEN_STRING,

    // Keywords
    TOKEN_CLEM, TOKEN_SCRIPT, TOKEN_CONSOLE, TOKEN_VAR, TOKEN_IF, TOKEN_THEN,
    TOKEN_ELSE, TOKEN_WHILE, TOKEN_DO, TOKEN_FOR, TOKEN_FROM, TOKEN_TO,
    TOKEN_COLOR, TOKEN_QUIZ, TOKEN_OPTIONS, TOKEN_ANSWER,
    // NEW KEYWORDS
    TOKEN_INPUT, TOKEN_EVEN, TOKEN_ODD, TOKEN_DELAY, TOKEN_AND, TOKEN_OR,
    TOKEN_NOT, TOKEN_LENGTH, TOKEN_RANDOM, TOKEN_MATH,

    // Operators
    TOKEN_PLUS, TOKEN_MINUS, TOKEN_ASTERISK, TOKEN_SLASH, TOKEN_ASSIGN,
    TOKEN_ARROW, // ->
    TOKEN_EQ, TOKEN_NEQ, TOKEN_LT, TOKEN_LE, TOKEN_GT, TOKEN_GE,

    // Delimiters
    TOKEN_LPAREN, TOKEN_RPAREN, TOKEN_LBRACE, TOKEN_RBRACE, TOKEN_SEMICOLON
} TokenType;

// Token Structure
typedef struct {
    TokenType type;
    char *lexeme; // Dynamically allocated
    int line;
} Token;

// --- AST Node Types ---

typedef enum {
    NODE_PROGRAM,
    NODE_INT_LITERAL,
    NODE_STRING_LITERAL,
    NODE_IDENTIFIER,
    NODE_BINARY_EXPR,
    // NEW: Unary expression for NOT
    NODE_UNARY_EXPR,
    NODE_CONSOLE_STMT,
    NODE_VAR_DECL,
    NODE_ASSIGN_STMT,
    NODE_IF_STMT,
    NODE_WHILE_STMT,
    NODE_FOR_STMT,
    NODE_COLOR_STMT,
    NODE_QUIZ_STMT,
    NODE_BLOCK_STMT, // Represents a block of statements { ... }
    // NEW STATEMENT/EXPRESSION TYPES
    NODE_INPUT_EXPR, // An expression that takes input and returns a string
    NODE_IS_EVEN_EXPR,
    NODE_IS_ODD_EXPR,
    NODE_DELAY_STMT,
    NODE_LENGTH_EXPR,
    NODE_RANDOM_EXPR,
    NODE_MATH_EXPR
} NodeType;

// Forward declaration for ASTNode
struct ASTNode;

// Value type for the interpreter (currently just integers and strings)
typedef enum {
    VALUE_INT,
    VALUE_STRING,
    VALUE_NULL // For void returns or uninitialized vars
} ValueType;

typedef struct {
    ValueType type;
    union {
        int int_val;
        char *string_val; // Dynamically allocated for strings
    } data;
} Value;

// Binary Expression (existing, also used for logical AND/OR)
typedef struct {
    struct ASTNode *left;
    TokenType operator; // +, -, *, /, ==, !=, <, >, <=, >=, AND, OR
    struct ASTNode *right;
} BinaryExpr;

// NEW: Unary Expression (for NOT)
typedef struct {
    TokenType operator; // NOT
    struct ASTNode *operand;
} UnaryExpr;

// Console Statement: Clem console Script -> <expression>
typedef struct {
    struct ASTNode *expression;
} ConsoleStmt;

// Variable Declaration: Clem var Script <name> = <expression>
typedef struct {
    char *name; // Dynamically allocated
    struct ASTNode *initializer;
} VarDecl;

// Assignment Statement: <name> = <expression>
typedef struct {
    char *name; // Dynamically allocated
    struct ASTNode *value;
} AssignStmt;

// If Statement: Clem if Script <condition> Clem then Script { ... } [Clem else Script { ... }]
typedef struct {
    struct ASTNode *condition;
    struct ASTNode *then_block;
    struct ASTNode *else_block; // Optional
} IfStmt;

// While Statement: Clem while Script <condition> Clem do Script { ... }
typedef struct {
    struct ASTNode *condition;
    struct ASTNode *body;
} WhileStmt;

// For Statement: Clem for Script <var> Clem from Script <start> Clem to Script <end> Clem do Script { ... }
typedef struct {
    char *iterator_var; // Dynamically allocated
    struct ASTNode *start_expr;
    struct ASTNode *end_expr;
    struct ASTNode *body;
} ForStmt;

// Color Statement: Clem color Script <color_name> <text_expression>
typedef struct {
    char *color_name; // e.g., "red", "blue" (Dynamically allocated)
    struct ASTNode *text_expression;
} ColorStmt;

// Quiz Statement: Clem quiz Script <question> Clem options Script <opt1> ... Clem answer Script <correct_idx>
typedef struct {
    struct ASTNode *question_expr;
    struct ASTNode **option_exprs; // Array of string literal expressions (Nodes holding string literals)
    int num_options;
    struct ASTNode *correct_answer_expr; // Integer literal expression (Node holding int literal)
} QuizStmt;

// Block Statement: { ... }
typedef struct {
    struct ASTNode **statements; // Array of statements (Nodes)
    int num_statements;
} BlockStmt;

// NEW: Input Expression: Clem input Script <prompt_expression>
typedef struct {
    struct ASTNode *prompt_expr;
} InputExpr;

// NEW: Even/Odd Expression: Clem even/odd Script <number_expression>
typedef struct {
    struct ASTNode *number_expr;
} EvenOddExpr;

// NEW: Delay Statement: Clem delay Script <milliseconds_expression>
typedef struct {
    struct ASTNode *milliseconds_expr;
} DelayStmt;

// NEW: Length Expression: Clem length Script <string_expression>
typedef struct {
    struct ASTNode *string_expr;
} LengthExpr;

// NEW: Random Expression: Clem random Script <min_expr> <max_expr>
typedef struct {
    struct ASTNode *min_expr;
    struct ASTNode *max_expr;
} RandomExpr;

// NEW: Math Expression: Clem math Script <operation> <number_expr>
typedef struct {
    char *operation; // "square", "sqrt", "abs"
    struct ASTNode *number_expr;
} MathExpr;


// Generic AST Node
typedef struct ASTNode {
    NodeType type;
    int line; // For error reporting
    union {
        int int_val; // For NODE_INT_LITERAL
        char *string_val; // For NODE_STRING_LITERAL
        char *identifier_name; // For NODE_IDENTIFIER
        BinaryExpr binary_expr;
        UnaryExpr unary_expr; // NEW for NOT
        ConsoleStmt console_stmt;
        VarDecl var_decl;
        AssignStmt assign_stmt;
        IfStmt if_stmt;
        WhileStmt while_stmt;
        ForStmt for_stmt;
        ColorStmt color_stmt;
        QuizStmt quiz_stmt;
        BlockStmt block_stmt; // For program and blocks
        // NEW
        InputExpr input_expr;
        EvenOddExpr even_odd_expr;
        DelayStmt delay_stmt;
        LengthExpr length_expr;
        RandomExpr random_expr;
        MathExpr math_expr;
    } data;
} ASTNode;

// --- Global Variables (Lexer State) ---
char *source_code;
int lexer_current_pos;
int lexer_current_line;

// Parser's current token
Token current_token; // This struct will hold the current token, its lexeme will be dynamically allocated

// --- Environment for variables ---
typedef struct EnvEntry {
    char *name; // Dynamically allocated
    Value value; // Value, string_val inside will be dynamically allocated
    struct EnvEntry *next;
} EnvEntry;

EnvEntry *global_env = NULL;

// --- Forward Declarations for Parser ---
ASTNode *parse_program();
ASTNode *parse_statement();
ASTNode *parse_clem_statement();
// Updated expression parsing hierarchy
ASTNode *parse_expression();       // Lowest precedence, starts with logical_or
ASTNode *parse_logical_or();
ASTNode *parse_logical_and();
ASTNode *parse_logical_not();      // Handles unary NOT
ASTNode *parse_equality();
ASTNode *parse_comparison();
ASTNode *parse_term();
ASTNode *parse_factor();
ASTNode *parse_primary();
ASTNode *parse_block();

// --- Error Reporting ---
void error(const char *message) {
    fprintf(stderr, "Error [Line %d]: %s\n", lexer_current_line, message); // Use lexer_current_line for general errors
    exit(1);
}

void syntax_error(const char *expected) {
    fprintf(stderr, "Syntax Error [Line %d]: Expected %s, but found '%s' (type %d)\n",
            current_token.line, expected, current_token.lexeme, current_token.type);
    exit(1);
}

// --- Memory Management Helpers ---

// Free a token's dynamically allocated lexeme
void free_token_lexeme(Token *token) {
    if (token && token->lexeme) {
        free(token->lexeme);
        token->lexeme = NULL; // Prevent double free
    }
}

// Create an AST node
ASTNode *create_node(NodeType type, int line) {
    ASTNode *node = (ASTNode *)malloc(sizeof(ASTNode));
    if (!node) {
        perror("Failed to allocate AST node");
        exit(1);
    }
    node->type = type;
    node->line = line;
    memset(&node->data, 0, sizeof(node->data)); // Initialize union
    return node;
}

// Recursively free an AST node and its children
void free_ast_node(ASTNode *node) {
    if (!node) return;

    switch (node->type) {
        case NODE_PROGRAM:
        case NODE_BLOCK_STMT:
            for (int i = 0; i < node->data.block_stmt.num_statements; ++i) {
                free_ast_node(node->data.block_stmt.statements[i]);
            }
            free(node->data.block_stmt.statements);
            break;
        case NODE_BINARY_EXPR:
            free_ast_node(node->data.binary_expr.left);
            free_ast_node(node->data.binary_expr.right);
            break;
        case NODE_UNARY_EXPR: // NEW
            free_ast_node(node->data.unary_expr.operand);
            break;
        case NODE_CONSOLE_STMT:
            free_ast_node(node->data.console_stmt.expression);
            break;
        case NODE_VAR_DECL:
            free(node->data.var_decl.name);
            free_ast_node(node->data.var_decl.initializer);
            break;
        case NODE_ASSIGN_STMT:
            free(node->data.assign_stmt.name);
            free_ast_node(node->data.assign_stmt.value);
            break;
        case NODE_IF_STMT:
            free_ast_node(node->data.if_stmt.condition);
            free_ast_node(node->data.if_stmt.then_block);
            free_ast_node(node->data.if_stmt.else_block);
            break;
        case NODE_WHILE_STMT:
            free_ast_node(node->data.while_stmt.condition);
            free_ast_node(node->data.while_stmt.body);
            break;
        case NODE_FOR_STMT:
            free(node->data.for_stmt.iterator_var);
            free_ast_node(node->data.for_stmt.start_expr);
            free_ast_node(node->data.for_stmt.end_expr);
            free_ast_node(node->data.for_stmt.body);
            break;
        case NODE_COLOR_STMT:
            free(node->data.color_stmt.color_name);
            free_ast_node(node->data.color_stmt.text_expression);
            break;
        case NODE_QUIZ_STMT:
            free_ast_node(node->data.quiz_stmt.question_expr);
            for (int i = 0; i < node->data.quiz_stmt.num_options; ++i) {
                free_ast_node(node->data.quiz_stmt.option_exprs[i]);
            }
            free(node->data.quiz_stmt.option_exprs);
            free_ast_node(node->data.quiz_stmt.correct_answer_expr);
            break;
        case NODE_INPUT_EXPR: // NEW
            free_ast_node(node->data.input_expr.prompt_expr);
            break;
        case NODE_IS_EVEN_EXPR: // NEW
        case NODE_IS_ODD_EXPR: // NEW
            free_ast_node(node->data.even_odd_expr.number_expr);
            break;
        case NODE_DELAY_STMT: // NEW
            free_ast_node(node->data.delay_stmt.milliseconds_expr);
            break;
        case NODE_LENGTH_EXPR: // NEW
            free_ast_node(node->data.length_expr.string_expr);
            break;
        case NODE_RANDOM_EXPR: // NEW
            free_ast_node(node->data.random_expr.min_expr);
            free_ast_node(node->data.random_expr.max_expr);
            break;
        case NODE_MATH_EXPR: // NEW
            free(node->data.math_expr.operation);
            free_ast_node(node->data.math_expr.number_expr);
            break;
        case NODE_STRING_LITERAL:
        case NODE_IDENTIFIER:
            free(node->data.string_val); // This union member also covers identifier_name
            break;
        case NODE_INT_LITERAL:
            // No dynamic memory to free for int_val
            break;
    }
    free(node);
}

// Free a Value's dynamically allocated string_val
void free_value(Value val) {
    if (val.type == VALUE_STRING) {
        free(val.data.string_val);
    }
}

// Free the entire environment
void free_environment(EnvEntry *env) {
    EnvEntry *current = env;
    while (current) {
        EnvEntry *next = current->next;
        free(current->name);
        free_value(current->value);
        free(current);
        current = next;
    }
}

// --- Lexer ---

static char lexer_peek_char() {
    if (lexer_current_pos >= strlen(source_code)) return '\0';
    return source_code[lexer_current_pos];
}

static char lexer_advance_char() {
    if (lexer_current_pos >= strlen(source_code)) return '\0';
    char c = source_code[lexer_current_pos];
    lexer_current_pos++;
    return c;
}

static void lexer_skip_whitespace_and_comments() {
    while (lexer_current_pos < strlen(source_code)) {
        char c = lexer_peek_char();
        if (isspace(c)) {
            if (c == '\n') lexer_current_line++;
            lexer_advance_char();
        } else if (c == '#') { // Comment until end of line
            while (lexer_peek_char() != '\n' && lexer_peek_char() != '\0') {
                lexer_advance_char();
            }
            if (lexer_peek_char() == '\n') { // Consume newline if present
                lexer_advance_char();
                lexer_current_line++;
            }
        } else {
            break;
        }
    }
}

// Helper to check for keyword
static TokenType lexer_check_keyword(const char *text) {
    if (strcmp(text, "Clem") == 0) return TOKEN_CLEM;
    if (strcmp(text, "Script") == 0) return TOKEN_SCRIPT;
    if (strcmp(text, "console") == 0) return TOKEN_CONSOLE;
    if (strcmp(text, "var") == 0) return TOKEN_VAR;
    if (strcmp(text, "if") == 0) return TOKEN_IF;
    if (strcmp(text, "then") == 0) return TOKEN_THEN;
    if (strcmp(text, "else") == 0) return TOKEN_ELSE;
    if (strcmp(text, "while") == 0) return TOKEN_WHILE;
    if (strcmp(text, "do") == 0) return TOKEN_DO;
    if (strcmp(text, "for") == 0) return TOKEN_FOR;
    if (strcmp(text, "from") == 0) return TOKEN_FROM;
    if (strcmp(text, "to") == 0) return TOKEN_TO;
    if (strcmp(text, "color") == 0) return TOKEN_COLOR;
    if (strcmp(text, "quiz") == 0) return TOKEN_QUIZ;
    if (strcmp(text, "options") == 0) return TOKEN_OPTIONS;
    if (strcmp(text, "answer") == 0) return TOKEN_ANSWER;
    // NEW KEYWORDS
    if (strcmp(text, "input") == 0) return TOKEN_INPUT;
    if (strcmp(text, "even") == 0) return TOKEN_EVEN;
    if (strcmp(text, "odd") == 0) return TOKEN_ODD;
    if (strcmp(text, "delay") == 0) return TOKEN_DELAY;
    if (strcmp(text, "and") == 0) return TOKEN_AND;
    if (strcmp(text, "or") == 0) return TOKEN_OR;
    if (strcmp(text, "not") == 0) return TOKEN_NOT;
    if (strcmp(text, "length") == 0) return TOKEN_LENGTH;
    if (strcmp(text, "random") == 0) return TOKEN_RANDOM;
    if (strcmp(text, "math") == 0) return TOKEN_MATH;
    return TOKEN_IDENTIFIER;
}

// Main lexer function: returns a Token struct by value
Token lexer_get_token_struct() {
    lexer_skip_whitespace_and_comments();

    if (lexer_current_pos >= strlen(source_code)) {
        return (Token){TOKEN_EOF, strdup("EOF"), lexer_current_line};
    }

    char c = lexer_peek_char();
    int start_pos = lexer_current_pos;
    int token_line = lexer_current_line;

    // Identifiers and Keywords
    if (isalpha(c) || c == '_') {
        while (isalnum(lexer_peek_char()) || lexer_peek_char() == '_') {
            lexer_advance_char();
        }
        char *lexeme = strndup(source_code + start_pos, lexer_current_pos - start_pos);
        TokenType type = lexer_check_keyword(lexeme);
        return (Token){type, lexeme, token_line};
    }

    // Numbers
    if (isdigit(c)) {
        while (isdigit(lexer_peek_char())) {
            lexer_advance_char();
        }
        char *lexeme = strndup(source_code + start_pos, lexer_current_pos - start_pos);
        return (Token){TOKEN_INT, lexeme, token_line};
    }

    // Strings
    if (c == '"') {
        lexer_advance_char(); // Consume '"'
        start_pos = lexer_current_pos; // Start recording lexeme after opening quote
        while (lexer_peek_char() != '"' && lexer_peek_char() != '\0' && lexer_peek_char() != '\n') {
            lexer_advance_char();
        }
        if (lexer_peek_char() == '\n' || lexer_peek_char() == '\0') {
            error("Unterminated string literal.");
        }
        char *lexeme = strndup(source_code + start_pos, lexer_current_pos - start_pos);
        lexer_advance_char(); // Consume closing '"'
        return (Token){TOKEN_STRING, lexeme, token_line};
    }

    // Operators and Delimiters
    lexer_advance_char(); // Consume the current character
    switch (c) {
        case '+': return (Token){TOKEN_PLUS, strdup("+"), token_line};
        case '-':
            if (lexer_peek_char() == '>') {
                lexer_advance_char();
                return (Token){TOKEN_ARROW, strdup("->"), token_line};
            }
            return (Token){TOKEN_MINUS, strdup("-"), token_line};
        case '*': return (Token){TOKEN_ASTERISK, strdup("*"), token_line};
        case '/': return (Token){TOKEN_SLASH, strdup("/"), token_line};
        case '=':
            if (lexer_peek_char() == '=') {
                lexer_advance_char();
                return (Token){TOKEN_EQ, strdup("=="), token_line};
            }
            return (Token){TOKEN_ASSIGN, strdup("="), token_line};
        case '!':
            if (lexer_peek_char() == '=') {
                lexer_advance_char();
                return (Token){TOKEN_NEQ, strdup("!="), token_line};
            }
            error("Unexpected character '!'");
        case '<':
            if (lexer_peek_char() == '=') {
                lexer_advance_char();
                return (Token){TOKEN_LE, strdup("<="), token_line};
            }
            return (Token){TOKEN_LT, strdup("<"), token_line};
        case '>':
            if (lexer_peek_char() == '=') {
                lexer_advance_char();
                return (Token){TOKEN_GE, strdup(">="), token_line};
            }
            return (Token){TOKEN_GT, strdup(">"), token_line};
        case '(': return (Token){TOKEN_LPAREN, strdup("("), token_line};
        case ')': return (Token){TOKEN_RPAREN, strdup(")"), token_line};
        case '{': return (Token){TOKEN_LBRACE, strdup("{"), token_line};
        case '}': return (Token){TOKEN_RBRACE, strdup("}"), token_line};
        case ';': return (Token){TOKEN_SEMICOLON, strdup(";"), token_line};
        default:
            fprintf(stderr, "Lexical Error [Line %d]: Unexpected character '%c'\n", token_line, c);
            exit(1);
    }
}

// Advance the global current_token to the next token
void advance_token() {
    free_token_lexeme(&current_token); // Free lexeme of previous token
    current_token = lexer_get_token_struct(); // Get new token struct and assign it
}

// Peeks the type of the next token without advancing the global current_token
TokenType peek_token_type() {
    int original_pos = lexer_current_pos;
    int original_line = lexer_current_line;
    
    Token peeked_token = lexer_get_token_struct();
    TokenType type = peeked_token.type;

    free_token_lexeme(&peeked_token); // Free lexeme of the peeked token

    lexer_current_pos = original_pos; // Restore lexer position
    lexer_current_line = original_line; // Restore lexer line

    return type;
}


// --- Parser ---

void consume(TokenType type, const char *expected_lexeme) {
    if (current_token.type == type) {
        advance_token(); // Move to the next token
    } else {
        syntax_error(expected_lexeme);
    }
}

// Entry point for parsing
ASTNode *parse_program() {
    ASTNode *program = create_node(NODE_PROGRAM, 0); // Line number might be inaccurate for program node
    program->data.block_stmt.statements = NULL;
    program->data.block_stmt.num_statements = 0;

    // Initialize the current_token for the parser
    current_token = lexer_get_token_struct(); // Get the very first token

    while (current_token.type != TOKEN_EOF) {
        ASTNode *stmt = parse_statement();
        if (stmt) {
            program->data.block_stmt.statements = (ASTNode **)realloc(
                program->data.block_stmt.statements,
                sizeof(ASTNode *) * (program->data.block_stmt.num_statements + 1)
            );
            if (!program->data.block_stmt.statements) {
                perror("Failed to reallocate statements in program");
                exit(1);
            }
            program->data.block_stmt.statements[program->data.block_stmt.num_statements++] = stmt;
        } else {
            // If parse_statement returned NULL and it's not EOF, it means an unparsable token.
            // This case should ideally be handled by syntax_error inside parse_statement.
            // But as a fallback:
            if (current_token.type != TOKEN_EOF) {
                 error("Unexpected token at program level. Possibly missing semicolon or incorrect syntax.");
            }
        }
    }
    return program;
}

ASTNode *parse_statement() {
    ASTNode *stmt = NULL;
    int stmt_line = current_token.line;

    if (current_token.type == TOKEN_CLEM) {
        stmt = parse_clem_statement();
    } else if (current_token.type == TOKEN_IDENTIFIER) {
        // Handle assignment statement: identifier = expression
        char *var_name = strdup(current_token.lexeme);
        consume(TOKEN_IDENTIFIER, "variable name");
        if (current_token.type == TOKEN_ASSIGN) {
            ASTNode *assign_node = create_node(NODE_ASSIGN_STMT, stmt_line);
            assign_node->data.assign_stmt.name = var_name;
            consume(TOKEN_ASSIGN, "=");
            assign_node->data.assign_stmt.value = parse_expression();
            stmt = assign_node;
        } else {
            free(var_name); // Free if not an assignment
            syntax_error("assignment operator '=' or Clem keyword expected after identifier.");
        }
    } else if (current_token.type == TOKEN_LBRACE) {
        // Handle standalone blocks (e.g. { stmt; stmt; })
        stmt = parse_block();
    } else {
        syntax_error("statement start (Clem keyword, identifier, or '{')");
    }

    // Optional semicolon at end of statement
    // NEW: Delay statement can be followed by a semicolon too.
    if (current_token.type == TOKEN_SEMICOLON) {
        consume(TOKEN_SEMICOLON, ";");
    }
    return stmt;
}

ASTNode *parse_clem_statement() {
    int stmt_line = current_token.line;
    consume(TOKEN_CLEM, "Clem");

    if (current_token.type == TOKEN_CONSOLE) {
        consume(TOKEN_CONSOLE, "console");
        consume(TOKEN_SCRIPT, "Script");
        consume(TOKEN_ARROW, "->"); // Clem console Script ->
        ASTNode *console_node = create_node(NODE_CONSOLE_STMT, stmt_line);
        console_node->data.console_stmt.expression = parse_expression();
        return console_node;
    } else if (current_token.type == TOKEN_VAR) {
        consume(TOKEN_VAR, "var");
        consume(TOKEN_SCRIPT, "Script");
        if (current_token.type != TOKEN_IDENTIFIER) {
            syntax_error("variable name");
        }
        char *var_name = strdup(current_token.lexeme);
        consume(TOKEN_IDENTIFIER, "variable name");
        ASTNode *var_decl_node = create_node(NODE_VAR_DECL, stmt_line);
        var_decl_node->data.var_decl.name = var_name;
        if (current_token.type == TOKEN_ASSIGN) {
            consume(TOKEN_ASSIGN, "=");
            var_decl_node->data.var_decl.initializer = parse_expression();
        } else {
            var_decl_node->data.var_decl.initializer = NULL; // Uninitialized variable
        }
        return var_decl_node;
    } else if (current_token.type == TOKEN_IF) {
        consume(TOKEN_IF, "if");
        consume(TOKEN_SCRIPT, "Script");
        ASTNode *if_node = create_node(NODE_IF_STMT, stmt_line);
        if_node->data.if_stmt.condition = parse_expression();
        consume(TOKEN_CLEM, "Clem");
        consume(TOKEN_THEN, "then");
        consume(TOKEN_SCRIPT, "Script");
        if_node->data.if_stmt.then_block = parse_block();
        
        // Check for optional "Clem else Script" block
        // If current token is TOKEN_CLEM, and the NEXT token is TOKEN_ELSE
        if (current_token.type == TOKEN_CLEM && peek_token_type() == TOKEN_ELSE) {
            consume(TOKEN_CLEM, "Clem"); // Consume the 'Clem'
            consume(TOKEN_ELSE, "else"); // Consume the 'else'
            consume(TOKEN_SCRIPT, "Script"); // Consume the 'Script'
            if_node->data.if_stmt.else_block = parse_block();
        } else {
            if_node->data.if_stmt.else_block = NULL;
        }
        return if_node;
    } else if (current_token.type == TOKEN_WHILE) {
        consume(TOKEN_WHILE, "while");
        consume(TOKEN_SCRIPT, "Script");
        ASTNode *while_node = create_node(NODE_WHILE_STMT, stmt_line);
        while_node->data.while_stmt.condition = parse_expression();
        consume(TOKEN_CLEM, "Clem");
        consume(TOKEN_DO, "do");
        consume(TOKEN_SCRIPT, "Script");
        while_node->data.while_stmt.body = parse_block();
        return while_node;
    } else if (current_token.type == TOKEN_FOR) {
        consume(TOKEN_FOR, "for");
        consume(TOKEN_SCRIPT, "Script");
        ASTNode *for_node = create_node(NODE_FOR_STMT, stmt_line);
        if (current_token.type != TOKEN_IDENTIFIER) {
            syntax_error("iterator variable name");
        }
        for_node->data.for_stmt.iterator_var = strdup(current_token.lexeme);
        consume(TOKEN_IDENTIFIER, "iterator variable name");
        consume(TOKEN_CLEM, "Clem");
        consume(TOKEN_FROM, "from");
        consume(TOKEN_SCRIPT, "Script");
        for_node->data.for_stmt.start_expr = parse_expression();
        consume(TOKEN_CLEM, "Clem");
        consume(TOKEN_TO, "to");
        consume(TOKEN_SCRIPT, "Script");
        for_node->data.for_stmt.end_expr = parse_expression();
        consume(TOKEN_CLEM, "Clem");
        consume(TOKEN_DO, "do");
        consume(TOKEN_SCRIPT, "Script");
        for_node->data.for_stmt.body = parse_block();
        return for_node;
    } else if (current_token.type == TOKEN_COLOR) {
        consume(TOKEN_COLOR, "color");
        consume(TOKEN_SCRIPT, "Script");
        if (!(current_token.type == TOKEN_IDENTIFIER || current_token.type == TOKEN_STRING)) {
            syntax_error("color name (e.g., red, blue) or string literal for color");
        }
        char *color_name = strdup(current_token.lexeme); // Capture the color name (e.g. "red" or "blue")
        consume(current_token.type, "color name or string literal"); // Consume the IDENTIFIER or STRING token
        
        ASTNode *color_node = create_node(NODE_COLOR_STMT, stmt_line);
        color_node->data.color_stmt.color_name = color_name;
        color_node->data.color_stmt.text_expression = parse_expression();
        return color_node;
    } else if (current_token.type == TOKEN_QUIZ) {
        consume(TOKEN_QUIZ, "quiz");
        consume(TOKEN_SCRIPT, "Script");
        ASTNode *quiz_node = create_node(NODE_QUIZ_STMT, stmt_line);
        quiz_node->data.quiz_stmt.question_expr = parse_expression();
        consume(TOKEN_CLEM, "Clem");
        consume(TOKEN_OPTIONS, "options");
        consume(TOKEN_SCRIPT, "Script");
        // Parse options (list of string literal expressions)
        quiz_node->data.quiz_stmt.option_exprs = NULL;
        quiz_node->data.quiz_stmt.num_options = 0;
        while (current_token.type == TOKEN_STRING) {
            quiz_node->data.quiz_stmt.option_exprs = (ASTNode **)realloc(
                quiz_node->data.quiz_stmt.option_exprs,
                sizeof(ASTNode *) * (quiz_node->data.quiz_stmt.num_options + 1)
            );
            if (!quiz_node->data.quiz_stmt.option_exprs) {
                perror("Failed to reallocate options for quiz");
                exit(1);
            }
            quiz_node->data.quiz_stmt.option_exprs[quiz_node->data.quiz_stmt.num_options++] = parse_primary(); // Should be a string literal
        }
        if (quiz_node->data.quiz_stmt.num_options == 0) {
            error("Quiz must have at least one option (string literal).");
        }
        consume(TOKEN_CLEM, "Clem");
        consume(TOKEN_ANSWER, "answer");
        consume(TOKEN_SCRIPT, "Script");
        quiz_node->data.quiz_stmt.correct_answer_expr = parse_primary(); // Should be an integer literal
        if (quiz_node->data.quiz_stmt.correct_answer_expr->type != NODE_INT_LITERAL) {
            error("Quiz answer must be an integer literal (option index).");
        }
        return quiz_node;
    }
    // NEW STATEMENT TYPE: Delay
    else if (current_token.type == TOKEN_DELAY) {
        consume(TOKEN_DELAY, "delay");
        consume(TOKEN_SCRIPT, "Script");
        ASTNode *delay_node = create_node(NODE_DELAY_STMT, stmt_line);
        delay_node->data.delay_stmt.milliseconds_expr = parse_expression();
        return delay_node;
    }
    
    syntax_error("valid ClemScript statement keyword (console, var, if, while, for, color, quiz, delay)");
    return NULL; // Should not reach here
}

ASTNode *parse_block() {
    ASTNode *block_node = create_node(NODE_BLOCK_STMT, current_token.line);
    block_node->data.block_stmt.statements = NULL;
    block_node->data.block_stmt.num_statements = 0;

    consume(TOKEN_LBRACE, "{");

    while (current_token.type != TOKEN_RBRACE && current_token.type != TOKEN_EOF) {
        ASTNode *stmt = parse_statement();
        if (stmt) {
            block_node->data.block_stmt.statements = (ASTNode **)realloc(
                block_node->data.block_stmt.statements,
                sizeof(ASTNode *) * (block_node->data.block_stmt.num_statements + 1)
            );
            if (!block_node->data.block_stmt.statements) {
                perror("Failed to reallocate statements in block");
                exit(1);
            }
            block_node->data.block_stmt.statements[block_node->data.block_stmt.num_statements++] = stmt;
        } else {
            if (current_token.type != TOKEN_RBRACE && current_token.type != TOKEN_EOF) {
                 error("Unexpected token inside block. Possibly missing semicolon or incorrect syntax.");
            }
        }
    }
    consume(TOKEN_RBRACE, "}");
    return block_node;
}


// Expression parsing (Operator Precedence Parsing)
// expression -> logical_or
// logical_or   -> logical_and ( ( "Clem or Script" ) logical_and )*
// logical_and  -> logical_not ( ( "Clem and Script" ) logical_not )*
// logical_not  -> "Clem not Script" comparison | comparison
// comparison -> term ( ( "==" | "!=" | ">" | ">=" | "<" | "<=" ) term )*
// term       -> factor ( ( "+" | "-" ) factor )*
// factor     -> primary ( ( "*" | "/" ) primary )*
// primary    -> INT | STRING | IDENTIFIER | "(" expression ")"
//             | Clem input Script <prompt>
//             | Clem even Script <number_expr>
//             | Clem odd Script <number_expr>
//             | Clem length Script <string_expr>
//             | Clem random Script <min_expr> <max_expr>

ASTNode *parse_expression() {
    return parse_logical_or();
}

ASTNode *parse_logical_or() {
    ASTNode *expr = parse_logical_and();
    while (current_token.type == TOKEN_CLEM && peek_token_type() == TOKEN_OR) {
        int op_line = current_token.line;
        consume(TOKEN_CLEM, "Clem");
        consume(TOKEN_OR, "or");
        consume(TOKEN_SCRIPT, "Script"); // Consume 'Script' for "Clem or Script"
        
        ASTNode *node = create_node(NODE_BINARY_EXPR, op_line);
        node->data.binary_expr.left = expr;
        node->data.binary_expr.operator = TOKEN_OR;
        node->data.binary_expr.right = parse_logical_and();
        expr = node;
    }
    return expr;
}

ASTNode *parse_logical_and() {
    ASTNode *expr = parse_logical_not();
    while (current_token.type == TOKEN_CLEM && peek_token_type() == TOKEN_AND) {
        int op_line = current_token.line;
        consume(TOKEN_CLEM, "Clem");
        consume(TOKEN_AND, "and");
        consume(TOKEN_SCRIPT, "Script"); // Consume 'Script' for "Clem and Script"

        ASTNode *node = create_node(NODE_BINARY_EXPR, op_line);
        node->data.binary_expr.left = expr;
        node->data.binary_expr.operator = TOKEN_AND;
        node->data.binary_expr.right = parse_logical_not();
        expr = node;
    }
    return expr;
}

ASTNode *parse_logical_not() {
    if (current_token.type == TOKEN_CLEM && peek_token_type() == TOKEN_NOT) {
        int op_line = current_token.line;
        consume(TOKEN_CLEM, "Clem");
        consume(TOKEN_NOT, "not");
        consume(TOKEN_SCRIPT, "Script"); // Consume 'Script' for "Clem not Script"

        ASTNode *node = create_node(NODE_UNARY_EXPR, op_line);
        node->data.unary_expr.operator = TOKEN_NOT;
        node->data.unary_expr.operand = parse_comparison(); // NOT applies to a comparison or higher
        return node;
    }
    return parse_equality(); // Next precedence level
}


ASTNode *parse_equality() {
    ASTNode *expr = parse_comparison();
    while (current_token.type == TOKEN_EQ || current_token.type == TOKEN_NEQ) {
        ASTNode *node = create_node(NODE_BINARY_EXPR, current_token.line);
        node->data.binary_expr.left = expr;
        node->data.binary_expr.operator = current_token.type;
        advance_token(); // Consume operator
        node->data.binary_expr.right = parse_comparison();
        expr = node;
    }
    return expr;
}

ASTNode *parse_comparison() {
    ASTNode *expr = parse_term();
    while (current_token.type == TOKEN_GT || current_token.type == TOKEN_GE ||
           current_token.type == TOKEN_LT || current_token.type == TOKEN_LE) {
        ASTNode *node = create_node(NODE_BINARY_EXPR, current_token.line);
        node->data.binary_expr.left = expr;
        node->data.binary_expr.operator = current_token.type;
        advance_token(); // Consume operator
        node->data.binary_expr.right = parse_term();
        expr = node;
    }
    return expr;
}

ASTNode *parse_term() {
    ASTNode *expr = parse_factor();
    while (current_token.type == TOKEN_PLUS || current_token.type == TOKEN_MINUS) {
        ASTNode *node = create_node(NODE_BINARY_EXPR, current_token.line);
        node->data.binary_expr.left = expr;
        node->data.binary_expr.operator = current_token.type;
        advance_token(); // Consume operator
        node->data.binary_expr.right = parse_factor();
        expr = node;
    }
    return expr;
}

ASTNode *parse_factor() {
    ASTNode *expr = parse_primary();
    while (current_token.type == TOKEN_ASTERISK || current_token.type == TOKEN_SLASH) {
        ASTNode *node = create_node(NODE_BINARY_EXPR, current_token.line);
        node->data.binary_expr.left = expr;
        node->data.binary_expr.operator = current_token.type;
        advance_token(); // Consume operator
        node->data.binary_expr.right = parse_primary();
        expr = node;
    }
    return expr;
}

ASTNode *parse_primary() {
    ASTNode *node = NULL;
    int node_line = current_token.line;

    if (current_token.type == TOKEN_INT) {
        node = create_node(NODE_INT_LITERAL, node_line);
        node->data.int_val = atoi(current_token.lexeme);
        consume(TOKEN_INT, "integer literal");
    } else if (current_token.type == TOKEN_STRING) {
        node = create_node(NODE_STRING_LITERAL, node_line);
        node->data.string_val = strdup(current_token.lexeme);
        consume(TOKEN_STRING, "string literal");
    } else if (current_token.type == TOKEN_IDENTIFIER) {
        node = create_node(NODE_IDENTIFIER, node_line);
        node->data.identifier_name = strdup(current_token.lexeme);
        consume(TOKEN_IDENTIFIER, "identifier");
    } else if (current_token.type == TOKEN_LPAREN) {
        consume(TOKEN_LPAREN, "(");
        node = parse_expression();
        consume(TOKEN_RPAREN, ")");
    }
    // NEW EXPRESSION TYPES
    else if (current_token.type == TOKEN_CLEM) {
        // Peek the next token to determine which ClemScript expression it is
        TokenType next_type = peek_token_type();
        if (next_type == TOKEN_INPUT) {
            consume(TOKEN_CLEM, "Clem");
            consume(TOKEN_INPUT, "input");
            consume(TOKEN_SCRIPT, "Script");
            node = create_node(NODE_INPUT_EXPR, node_line);
            node->data.input_expr.prompt_expr = parse_expression(); // Prompt can be an expression
            return node;
        } else if (next_type == TOKEN_EVEN) {
            consume(TOKEN_CLEM, "Clem");
            consume(TOKEN_EVEN, "even");
            consume(TOKEN_SCRIPT, "Script");
            node = create_node(NODE_IS_EVEN_EXPR, node_line);
            node->data.even_odd_expr.number_expr = parse_expression();
            return node;
        } else if (next_type == TOKEN_ODD) {
            consume(TOKEN_CLEM, "Clem");
            consume(TOKEN_ODD, "odd");
            consume(TOKEN_SCRIPT, "Script");
            node = create_node(NODE_IS_ODD_EXPR, node_line);
            node->data.even_odd_expr.number_expr = parse_expression();
            return node;
        } else if (next_type == TOKEN_LENGTH) {
            consume(TOKEN_CLEM, "Clem");
            consume(TOKEN_LENGTH, "length");
            consume(TOKEN_SCRIPT, "Script");
            node = create_node(NODE_LENGTH_EXPR, node_line);
            node->data.length_expr.string_expr = parse_expression();
            return node;
        } else if (next_type == TOKEN_RANDOM) {
            consume(TOKEN_CLEM, "Clem");
            consume(TOKEN_RANDOM, "random");
            consume(TOKEN_SCRIPT, "Script");
            node = create_node(NODE_RANDOM_EXPR, node_line);
            node->data.random_expr.min_expr = parse_expression();
            node->data.random_expr.max_expr = parse_expression();
            return node;
        } else if (next_type == TOKEN_MATH) {
            consume(TOKEN_CLEM, "Clem");
            consume(TOKEN_MATH, "math");
            consume(TOKEN_SCRIPT, "Script");
            node = create_node(NODE_MATH_EXPR, node_line);
            if (current_token.type != TOKEN_IDENTIFIER) {
                syntax_error("math operation (square, sqrt, abs)");
            }
            node->data.math_expr.operation = strdup(current_token.lexeme);
            consume(TOKEN_IDENTIFIER, "math operation");
            node->data.math_expr.number_expr = parse_expression();
            return node;
        } else {
            // If it was Clem but not a recognized special expression, it must be part of a statement
            // or an error. Re-add Clem to be consumed by parse_clem_statement
            syntax_error("expected integer, string, identifier, '(', or a ClemScript expression (input, even, odd, length, random)");
        }
    }
    else {
        syntax_error("expected integer, string, identifier, or '('");
    }
    return node;
}

// --- Environment Management ---

EnvEntry *env_lookup(const char *name) {
    EnvEntry *current = global_env;
    while (current) {
        if (strcmp(current->name, name) == 0) {
            return current;
        }
        current = current->next;
    }
    return NULL;
}

void env_assign(const char *name, Value value) {
    EnvEntry *entry = env_lookup(name);
    if (entry) {
        // Free old value if it was a string before assigning new one
        if (entry->value.type == VALUE_STRING) {
            free(entry->value.data.string_val);
        }
        // Assign new value (string_val in 'value' needs to be a new strdup)
        if (value.type == VALUE_STRING && value.data.string_val != NULL) {
            entry->value.type = VALUE_STRING;
            entry->value.data.string_val = strdup(value.data.string_val);
        } else {
            entry->value = value;
        }
    } else {
        // New variable
        EnvEntry *new_entry = (EnvEntry *)malloc(sizeof(EnvEntry));
        if (!new_entry) {
            perror("Failed to allocate environment entry");
            exit(1);
        }
        new_entry->name = strdup(name);
        // If the value being assigned is a string, duplicate it to own the memory
        if (value.type == VALUE_STRING && value.data.string_val != NULL) {
            new_entry->value.type = VALUE_STRING;
            new_entry->value.data.string_val = strdup(value.data.string_val);
        } else {
            new_entry->value = value;
        }
        new_entry->next = global_env;
        global_env = new_entry;
    }
}

// --- Evaluator ---

// Utility to print colored text
void print_colored_text(const char *color_name, const char *text) {
    const char *color_code = ANSI_COLOR_RESET; // Default to reset

    if (strcasecmp(color_name, "red") == 0) color_code = ANSI_COLOR_RED;
    else if (strcasecmp(color_name, "green") == 0) color_code = ANSI_COLOR_GREEN;
    else if (strcasecmp(color_name, "yellow") == 0) color_code = ANSI_COLOR_YELLOW;
    else if (strcasecmp(color_name, "blue") == 0) color_code = ANSI_COLOR_BLUE;
    else if (strcasecmp(color_name, "magenta") == 0) color_code = ANSI_COLOR_MAGENTA;
    else if (strcasecmp(color_name, "cyan") == 0) color_code = ANSI_COLOR_CYAN;
    else if (strcasecmp(color_name, "white") == 0) color_code = ANSI_COLOR_WHITE;
    // Add more colors as needed. Default is no color if not found.

    printf("%s%s%s\n", color_code, text, ANSI_COLOR_RESET);
}

// Helper for delaying execution
void perform_delay(long milliseconds) {
    if (milliseconds < 0) {
        fprintf(stderr, "Runtime Error: Delay duration cannot be negative.\n");
        return;
    }
#ifdef _WIN32
    Sleep(milliseconds); // milliseconds
#else
    // usleep takes microseconds, so multiply by 1000
    // nanosleep is more precise for higher values, but usleep is simpler for small delays.
    // For milliseconds, usleep might be good enough.
    usleep(milliseconds * 1000); 
#endif
}


// Main evaluation function
Value evaluate(ASTNode *node) {
    if (!node) {
        Value null_val;
        null_val.type = VALUE_NULL;
        return null_val;
    }

    Value result;
    result.type = VALUE_NULL; // Default return value

    switch (node->type) {
        case NODE_INT_LITERAL:
            result.type = VALUE_INT;
            result.data.int_val = node->data.int_val;
            return result;
        case NODE_STRING_LITERAL:
            result.type = VALUE_STRING;
            result.data.string_val = strdup(node->data.string_val); // Duplicate string for safety
            return result;
        case NODE_IDENTIFIER: {
            EnvEntry *entry = env_lookup(node->data.identifier_name);
            if (!entry) {
                fprintf(stderr, "Runtime Error [Line %d]: Undefined variable '%s'\n", node->line, node->data.identifier_name);
                exit(1);
            }
            // Return a copy for strings to avoid dangling pointers if original is freed later
            if (entry->value.type == VALUE_STRING) {
                result.type = VALUE_STRING;
                result.data.string_val = strdup(entry->value.data.string_val);
            } else {
                result = entry->value;
            }
            return result;
        }
        case NODE_BINARY_EXPR: {
            Value left_val = evaluate(node->data.binary_expr.left);
            // Short-circuiting for logical AND/OR
            if (node->data.binary_expr.operator == TOKEN_OR) {
                if (left_val.type == VALUE_INT && left_val.data.int_val != 0) {
                    return left_val; // True, short-circuit OR
                }
            } else if (node->data.binary_expr.operator == TOKEN_AND) {
                if (left_val.type == VALUE_INT && left_val.data.int_val == 0) {
                    return left_val; // False, short-circuit AND
                }
            }

            Value right_val = evaluate(node->data.binary_expr.right);

            // Handle string concatenation for TOKEN_PLUS
            if (node->data.binary_expr.operator == TOKEN_PLUS &&
                (left_val.type == VALUE_STRING || right_val.type == VALUE_STRING)) {
                
                char *str_left = NULL;
                char *str_right = NULL;
                
                if (left_val.type == VALUE_STRING) {
                    str_left = left_val.data.string_val;
                } else if (left_val.type == VALUE_INT) {
                    char buf[32];
                    snprintf(buf, sizeof(buf), "%d", left_val.data.int_val);
                    str_left = strdup(buf);
                } else {
                    fprintf(stderr, "Runtime Error [Line %d]: Unsupported left type for concatenation.\n", node->line);
                    exit(1);
                }

                if (right_val.type == VALUE_STRING) {
                    str_right = right_val.data.string_val;
                } else if (right_val.type == VALUE_INT) {
                    char buf[32];
                    snprintf(buf, sizeof(buf), "%d", right_val.data.int_val);
                    str_right = strdup(buf);
                } else {
                    fprintf(stderr, "Runtime Error [Line %d]: Unsupported right type for concatenation.\n", node->line);
                    exit(1);
                }

                size_t new_len = strlen(str_left) + strlen(str_right) + 1;
                char *new_str = (char *)malloc(new_len);
                if (!new_str) {
                    perror("Failed to allocate string for concatenation");
                    exit(1);
                }
                strcpy(new_str, str_left);
                strcat(new_str, str_right);

                // Free temporary strings generated from integers or original string values
                if (left_val.type == VALUE_INT) free(str_left); 
                else free_value(left_val); // Free original string if it was string type
                
                if (right_val.type == VALUE_INT) free(str_right);
                else free_value(right_val); // Free original string if it was string type

                result.type = VALUE_STRING;
                result.data.string_val = new_str;
                return result;
            }

            // Logical AND/OR operations
            if (node->data.binary_expr.operator == TOKEN_AND || node->data.binary_expr.operator == TOKEN_OR) {
                if (left_val.type != VALUE_INT || right_val.type != VALUE_INT) {
                    fprintf(stderr, "Runtime Error [Line %d]: Logical operators 'and', 'or' require integer operands.\n", node->line);
                    exit(1);
                }
                result.type = VALUE_INT;
                if (node->data.binary_expr.operator == TOKEN_AND) {
                    result.data.int_val = (left_val.data.int_val != 0 && right_val.data.int_val != 0);
                } else { // TOKEN_OR
                    result.data.int_val = (left_val.data.int_val != 0 || right_val.data.int_val != 0);
                }
                return result;
            }


            // For all other binary operations, both operands must be integers
            if (left_val.type != VALUE_INT || right_val.type != VALUE_INT) {
                fprintf(stderr, "Runtime Error [Line %d]: Type mismatch in binary operation. Expected integers for non-concatenation operations.\n", node->line);
                exit(1);
            }

            int left_int = left_val.data.int_val;
            int right_int = right_val.data.int_val;

            result.type = VALUE_INT; // All arithmetic and comparison results are integers (0 or 1 for boolean)
            switch (node->data.binary_expr.operator) {
                case TOKEN_PLUS: result.data.int_val = left_int + right_int; break;
                case TOKEN_MINUS: result.data.int_val = left_int - right_int; break;
                case TOKEN_ASTERISK: result.data.int_val = left_int * right_int; break;
                case TOKEN_SLASH:
                    if (right_int == 0) {
                        fprintf(stderr, "Runtime Error [Line %d]: Division by zero.\n", node->line);
                        exit(1);
                    }
                    result.data.int_val = left_int / right_int;
                    break;
                case TOKEN_EQ: result.data.int_val = (left_int == right_int); break;
                case TOKEN_NEQ: result.data.int_val = (left_int != right_int); break;
                case TOKEN_LT: result.data.int_val = (left_int < right_int); break;
                case TOKEN_LE: result.data.int_val = (left_int <= right_int); break;
                case TOKEN_GT: result.data.int_val = (left_int > right_int); break;
                case TOKEN_GE: result.data.int_val = (left_int >= right_int); break;
                default:
                    fprintf(stderr, "Runtime Error [Line %d]: Unknown binary operator.\n", node->line);
                    exit(1);
            }
            return result;
        }
        case NODE_UNARY_EXPR: { // NEW
            Value operand_val = evaluate(node->data.unary_expr.operand);
            if (node->data.unary_expr.operator == TOKEN_NOT) {
                if (operand_val.type != VALUE_INT) {
                    fprintf(stderr, "Runtime Error [Line %d]: Unary operator 'not' requires an integer operand.\n", node->line);
                    exit(1);
                }
                result.type = VALUE_INT;
                result.data.int_val = (operand_val.data.int_val == 0); // NOT 0 is 1, NOT non-0 is 0
                return result;
            } else {
                fprintf(stderr, "Runtime Error [Line %d]: Unknown unary operator.\n", node->line);
                exit(1);
            }
        }
        case NODE_CONSOLE_STMT: {
            Value expr_val = evaluate(node->data.console_stmt.expression);
            if (expr_val.type == VALUE_INT) {
                printf("%d\n", expr_val.data.int_val);
            } else if (expr_val.type == VALUE_STRING) {
                printf("%s\n", expr_val.data.string_val);
                free(expr_val.data.string_val); // Free the duplicated string
            } else {
                fprintf(stderr, "Runtime Error [Line %d]: Cannot print value of unknown type.\n", node->line);
                exit(1);
            }
            break;
        }
        case NODE_VAR_DECL: {
            Value initial_val;
            if (node->data.var_decl.initializer) {
                initial_val = evaluate(node->data.var_decl.initializer);
            } else {
                initial_val.type = VALUE_NULL; // Default uninitialized to NULL
            }
            env_assign(node->data.var_decl.name, initial_val);
            // If the initial_val was a string, env_assign made its own copy, so free this one.
            if (initial_val.type == VALUE_STRING) free_value(initial_val);
            break;
        }
        case NODE_ASSIGN_STMT: {
            Value val = evaluate(node->data.assign_stmt.value);
            EnvEntry *entry = env_lookup(node->data.assign_stmt.name);
            if (!entry) {
                fprintf(stderr, "Runtime Error [Line %d]: Attempt to assign to undeclared variable '%s'.\n", node->line, node->data.assign_stmt.name);
                exit(1);
            }
            
            // Handle memory for old value
            if (entry->value.type == VALUE_STRING) {
                free(entry->value.data.string_val);
            }
            
            // Assign the new value. If it's a string, duplicate it for the environment.
            if (val.type == VALUE_STRING) {
                entry->value.type = VALUE_STRING;
                entry->value.data.string_val = strdup(val.data.string_val);
                free_value(val); // Free the temporary string returned by evaluate()
            } else {
                entry->value = val;
            }
            break;
        }
        case NODE_IF_STMT: {
            Value condition_val = evaluate(node->data.if_stmt.condition);
            if (condition_val.type != VALUE_INT) {
                fprintf(stderr, "Runtime Error [Line %d]: Condition must evaluate to an integer (boolean).\n", node->line);
                exit(1);
            }
            if (condition_val.data.int_val != 0) { // Non-zero is true
                evaluate(node->data.if_stmt.then_block);
            } else if (node->data.if_stmt.else_block) {
                evaluate(node->data.if_stmt.else_block);
            }
            break;
        }
        case NODE_WHILE_STMT: {
            while (1) {
                Value condition_val = evaluate(node->data.while_stmt.condition);
                if (condition_val.type != VALUE_INT) {
                    fprintf(stderr, "Runtime Error [Line %d]: Condition must evaluate to an integer (boolean).\n", node->line);
                    exit(1);
                }
                if (condition_val.data.int_val == 0) { // Zero is false
                    break;
                }
                evaluate(node->data.while_stmt.body);
            }
            break;
        }
        case NODE_FOR_STMT: {
            Value start_val = evaluate(node->data.for_stmt.start_expr);
            Value end_val = evaluate(node->data.for_stmt.end_expr);
            if (start_val.type != VALUE_INT || end_val.type != VALUE_INT) {
                fprintf(stderr, "Runtime Error [Line %d]: For loop range must be integers.\n", node->line);
                exit(1);
            }
            // Initialize iterator variable
            Value iterator_init_val;
            iterator_init_val.type = VALUE_INT;
            iterator_init_val.data.int_val = start_val.data.int_val;
            //env_assign makes a copy, so free the temporary initial_val if it was string (not the case here)
            env_assign(node->data.for_stmt.iterator_var, iterator_init_val); 

            // Get a pointer to the variable's entry in the environment for direct modification
            EnvEntry *iterator_entry = env_lookup(node->data.for_stmt.iterator_var);
            if (!iterator_entry) { 
                 fprintf(stderr, "Runtime Error [Line %d]: Internal error: For loop iterator not found in environment.\n", node->line);
                 exit(1);
            }

            for (; iterator_entry->value.data.int_val <= end_val.data.int_val; iterator_entry->value.data.int_val++) {
                evaluate(node->data.for_stmt.body);
            }
            break;
        }
        case NODE_COLOR_STMT: {
            // The color name is already a strdup'd C string from parsing
            char *color_name_str = node->data.color_stmt.color_name; 
            
            Value text_val = evaluate(node->data.color_stmt.text_expression);
            char *text_to_print = NULL;
            if (text_val.type == VALUE_STRING) {
                text_to_print = text_val.data.string_val;
            } else if (text_val.type == VALUE_INT) {
                char buf[256];
                snprintf(buf, sizeof(buf), "%d", text_val.data.int_val);
                text_to_print = strdup(buf); // Duplicate for printing, will be freed
            } else {
                fprintf(stderr, "Runtime Error [Line %d]: Text to color must be a string or integer.\n", node->line);
                exit(1);
            }

            print_colored_text(color_name_str, text_to_print);
            
            if (text_val.type == VALUE_INT) free(text_to_print); // Free if it was converted from int
            else free_value(text_val); // Free the duplicated string if it was string type
            break;
        }
        case NODE_QUIZ_STMT: {
            Value question_val = evaluate(node->data.quiz_stmt.question_expr);
            if (question_val.type != VALUE_STRING) {
                fprintf(stderr, "Runtime Error [Line %d]: Quiz question must be a string.\n", node->line);
                exit(1);
            }
            printf("\n--- ClemScript Quiz ---\n");
            printf("Question: %s\n", question_val.data.string_val);
            free_value(question_val); // Free duplicated string

            for (int i = 0; i < node->data.quiz_stmt.num_options; ++i) {
                Value option_val = evaluate(node->data.quiz_stmt.option_exprs[i]);
                if (option_val.type != VALUE_STRING) {
                    fprintf(stderr, "Runtime Error [Line %d]: Quiz option must be a string.\n", node->line);
                    exit(1);
                }
                printf("%d. %s\n", i + 1, option_val.data.string_val);
                free_value(option_val); // Free duplicated string
            }

            Value correct_answer_val = evaluate(node->data.quiz_stmt.correct_answer_expr);
            if (correct_answer_val.type != VALUE_INT) {
                fprintf(stderr, "Runtime Error [Line %d]: Quiz correct answer must be an integer (option index).\n", node->line);
                exit(1);
            }
            int correct_idx = correct_answer_val.data.int_val;

            int user_answer;
            printf("Enter your answer (1-%d): ", node->data.quiz_stmt.num_options);
            // Loop until valid integer input is received
            char input_buffer[256]; // A buffer for reading line input
            char *endptr;
            long num_read;
            
            while (fgets(input_buffer, sizeof(input_buffer), stdin) != NULL) {
                // Remove trailing newline character if present
                input_buffer[strcspn(input_buffer, "\n")] = 0;

                // Try to convert string to long integer
                num_read = strtol(input_buffer, &endptr, 10);

                // Check for conversion errors
                if (endptr == input_buffer || *endptr != '\0') {
                    printf("Invalid input. Please enter a number between 1 and %d: ", node->data.quiz_stmt.num_options);
                } else {
                    user_answer = (int)num_read;
                    if (user_answer >= 1 && user_answer <= node->data.quiz_stmt.num_options) {
                        break; // Valid input
                    } else {
                        printf("Input out of range. Please enter a number between 1 and %d: ", node->data.quiz_stmt.num_options);
                    }
                }
            }


            if (user_answer == correct_idx) {
                printf(ANSI_COLOR_GREEN "Correct!\n" ANSI_COLOR_RESET);
            } else {
                printf(ANSI_COLOR_RED "Incorrect. The correct answer was %d.\n" ANSI_COLOR_RESET, correct_idx);
            }
            printf("-----------------------\n");
            break;
        }
        case NODE_INPUT_EXPR: { // NEW
            Value prompt_val = evaluate(node->data.input_expr.prompt_expr);
            if (prompt_val.type != VALUE_STRING) {
                fprintf(stderr, "Runtime Error [Line %d]: Input prompt must be a string.\n", node->line);
                exit(1);
            }
            printf("%s", prompt_val.data.string_val);
            free_value(prompt_val); // Free the prompt string

            char buffer[1024]; // Arbitrary buffer size for input
            if (fgets(buffer, sizeof(buffer), stdin) != NULL) {
                buffer[strcspn(buffer, "\n")] = 0; // Remove trailing newline
                result.type = VALUE_STRING;
                result.data.string_val = strdup(buffer);
            } else {
                fprintf(stderr, "Runtime Error [Line %d]: Failed to read input.\n", node->line);
                exit(1);
            }
            return result;
        }
        case NODE_IS_EVEN_EXPR: { // NEW
            Value num_val = evaluate(node->data.even_odd_expr.number_expr);
            if (num_val.type != VALUE_INT) {
                fprintf(stderr, "Runtime Error [Line %d]: 'even' check requires an integer operand.\n", node->line);
                exit(1);
            }
            result.type = VALUE_INT;
            result.data.int_val = (num_val.data.int_val % 2 == 0);
            return result;
        }
        case NODE_IS_ODD_EXPR: { // NEW
            Value num_val = evaluate(node->data.even_odd_expr.number_expr);
            if (num_val.type != VALUE_INT) {
                fprintf(stderr, "Runtime Error [Line %d]: 'odd' check requires an integer operand.\n", node->line);
                exit(1);
            }
            result.type = VALUE_INT;
            result.data.int_val = (num_val.data.int_val % 2 != 0);
            return result;
        }
        case NODE_DELAY_STMT: { // NEW
            Value ms_val = evaluate(node->data.delay_stmt.milliseconds_expr);
            if (ms_val.type != VALUE_INT) {
                fprintf(stderr, "Runtime Error [Line %d]: Delay duration must be an integer (milliseconds).\n", node->line);
                exit(1);
            }
            perform_delay(ms_val.data.int_val);
            break;
        }
        case NODE_LENGTH_EXPR: { // NEW
            Value str_val = evaluate(node->data.length_expr.string_expr);
            if (str_val.type != VALUE_STRING) {
                fprintf(stderr, "Runtime Error [Line %d]: 'length' requires a string operand.\n", node->line);
                exit(1);
            }
            result.type = VALUE_INT;
            result.data.int_val = (int)strlen(str_val.data.string_val);
            free_value(str_val); // Free the duplicated string
            return result;
        }
        case NODE_RANDOM_EXPR: { // NEW
            Value min_val = evaluate(node->data.random_expr.min_expr);
            Value max_val = evaluate(node->data.random_expr.max_expr);
            if (min_val.type != VALUE_INT || max_val.type != VALUE_INT) {
                fprintf(stderr, "Runtime Error [Line %d]: 'random' requires integer min and max operands.\n", node->line);
                exit(1);
            }
            int min = min_val.data.int_val;
            int max = max_val.data.int_val;

            if (min > max) {
                fprintf(stderr, "Runtime Error [Line %d]: 'random' min value cannot be greater than max value.\n", node->line);
                exit(1);
            }
            
            result.type = VALUE_INT;
            result.data.int_val = rand() % (max - min + 1) + min;
            return result;
        }
        case NODE_MATH_EXPR: { // NEW
            Value num_val = evaluate(node->data.math_expr.number_expr);
            if (num_val.type != VALUE_INT) {
                fprintf(stderr, "Runtime Error [Line %d]: Math operations require an integer operand.\n", node->line);
                exit(1);
            }
            result.type = VALUE_INT;
            
            if (strcmp(node->data.math_expr.operation, "square") == 0) {
                result.data.int_val = num_val.data.int_val * num_val.data.int_val;
            } else if (strcmp(node->data.math_expr.operation, "sqrt") == 0) {
                if (num_val.data.int_val < 0) {
                    fprintf(stderr, "Runtime Error [Line %d]: Cannot calculate square root of negative number.\n", node->line);
                    exit(1);
                }
                result.data.int_val = (int)sqrt(num_val.data.int_val);
            } else if (strcmp(node->data.math_expr.operation, "abs") == 0) {
                result.data.int_val = abs(num_val.data.int_val);
            } else {
                fprintf(stderr, "Runtime Error [Line %d]: Unknown math operation '%s'. Supported: square, sqrt, abs.\n", 
                        node->line, node->data.math_expr.operation);
                exit(1);
            }
            return result;
        }
        case NODE_PROGRAM:
        case NODE_BLOCK_STMT: {
            for (int i = 0; i < node->data.block_stmt.num_statements; ++i) {
                evaluate(node->data.block_stmt.statements[i]);
            }
            break;
        }
        default:
            fprintf(stderr, "Runtime Error [Line %d]: Unknown AST node type %d.\n", node->line, node->type);
            exit(1);
    }
    return result; 
}

// --- Main Function ---

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <filename.clem>\n", argv[0]);
        return 1;
    }

    // Seed the random number generator once at the start
    srand((unsigned int)time(NULL));

    FILE *file = fopen(argv[1], "r");
    if (!file) {
        perror("Could not open file");
        return 1;
    }

    // Read file content into source_code buffer
    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);
    source_code = (char *)malloc(length + 1);
    if (!source_code) {
        perror("Failed to allocate source code buffer");
        fclose(file);
        return 1;
    }
    fread(source_code, 1, length, file);
    source_code[length] = '\0'; // Null-terminate
    fclose(file);

    lexer_current_pos = 0;
    lexer_current_line = 1;

    ASTNode *program_ast = NULL;

    // Lexing and Parsing
    // parse_program will call advance_token initially to get the first token
    program_ast = parse_program();

    // Evaluation
    evaluate(program_ast);

    // Cleanup
    free_ast_node(program_ast);
    free(source_code);
    free_environment(global_env);
    free_token_lexeme(&current_token); // Free the last token's lexeme

    return 0;
}
