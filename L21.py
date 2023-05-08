# 编写简单解释器进阶程序代码

# 自己写的支持一位数减法\支持两个操作数字之间空格\支持任意位数字长度
# Myversion  2021-06-10

# 支持减法、空格和任意位数字长度等功能的程序，修改为按鲁斯兰的方法实现
# Stdversion  2021-06-15

# 自己写的支持连续的加减法:如12 + 8 – 6 – 10
# Myversion  2021-06-16

# 支持连续的加减法:如12 + 8 – 6 – 10,修改为按鲁斯兰的方法实现
# Stdversion  2021-06-17

# 自己写的支持连续的乘除法法，如：7 * 4 / 2 * 3
# Myversion  2021-06-17

# 支持连续的乘除法法，修改为按鲁斯兰的方法实现：把词法分析器独立成单独的类
# Stdversion  2021-06-17

# 自己写的支持任意加、减、乘、除混合运算
# Myversion  2021-06-17

# 支持任意加、减、乘、除混合运算,修改为按鲁斯兰的方法实现
# 将加减、乘除、数字分别抽象为:expr(),term(),factor()
# Stdversion  2021-06-18

# 自己写支持括号的四则运算:7 + 3 * (10 / (12 / (3 + 1) - 1))
# 但我自己写却失败了！
# Myversion  2021-06-18

# 按鲁斯兰的方法实现括号的四则运算:7 + 3 * (10 / (12 / (3 + 1) - 1))
#  Stdversion  2021-06-18

# 自己写的：将词法解析器独立成Lexer,语法解析器独立成Parser,解释器独立成Interpreter
# 后一级的输入为前一级的输出:lexer=Parser(text),parser=Parser(lexer),interpreter=Interpreter(parser)
# Myversion  2021-06-21

# 自己写的：将语法解析器Parser通过"抽象语法树"来实现:支持连续加减和乘除的混合运算
# Myversion  2021-06-22

# 按鲁斯兰的教程：将语法解析器Parser通过"抽象语法树"AST来实现
# Stdversion  2021-06-29

# 按鲁斯兰的教程：语法解析器Parser支持一元计算符
# Stdversion  2021-06-30

# 按鲁斯兰的教程：支持变量
# Stdversion  2021-07-05

# 自己写的：BEGIN...END支持大小写、“div”关键字整数除法、变量可以使用下划线开头
# Myversion  2021-07-06

# 自己写的：支持程序头、变量声明明和程序体等完整程序结构;支持浮点数;支持注释
# Myversion  2021-07-07

# 自己写的：做进一步优化,程序头区和变量声明区也全部装进AST树
# Myversion  2021-07-08

# 按鲁斯兰的教程：支持完整的程序结构
# Stdversion  2021-07-09

# 自己写的：用Pyecharts把AST语法树图形可视化
# Myversion  2021-07-10

# 按鲁斯兰的教程：重构变量符号控制
# Stdversion  2021-07-12

# 按鲁斯兰的教程：支持过程声明
# Stdversion  2021-07-14

# 按鲁斯兰的教程：增加语义分析器
# Stdversion  2021-07-15

# 按鲁斯兰的教程：关键词支持不区分大小写
# Stdversion  2021-07-16

# 按鲁斯兰的教程：过程（函数）支持参数
# Stdversion  2021-07-17

# 按鲁斯兰的教程：变量作用域
# Stdversion  2021-07-19

# 按鲁斯兰的教程：规范解释器报错提示格式
# Stdversion  2021-07-22

# 按鲁斯兰的教程：支持过程（函数）调用
# Stdversion  2021-07-26

# 按鲁斯兰的教程：Call stack(调用栈) 和Activation Records（活动记录）
# Stdversion  2021-07-28

# 按鲁斯兰的教程：执行过程调用
# Stdversion  2021-07-29

import json
import os
import sys
from collections import OrderedDict
from pyecharts import options as opts
from pyecharts.charts import Page, Tree
from enum import Enum


class ErrorCode(Enum):
    UNEXPECTED_TOKEN = 'Unexpected token'
    ID_NOT_FOUND = 'Identifier not found'
    DUPLICATE_ID = 'Duplicate id found'


class Error(Exception):
    def __init__(self, error_code=None, token=None, message=None):
        self.error_code = error_code
        self.token = token
        # add exception class name before the message
        self.message = f'{self.__class__.__name__}: {message}'
        print(self.message)


class LexerError(Error):
    pass


class ParserError(Error):
    pass


class SemanticError(Error):
    pass


class TokenType(Enum):
    # single-character token types
    PLUS = '+'
    MINUS = '-'
    MUL = '*'
    FLOAT_DIV = '/'
    LPAREN = '('
    RPAREN = ')'
    SEMI = ';'
    DOT = '.'
    COLON = ':'
    COMMA = ','
    # block of reserved words
    PROGRAM = 'PROGRAM'  # marks the beginning of the block
    INTEGER = 'INTEGER'
    REAL = 'REAL'
    INTEGER_DIV = 'DIV'
    VAR = 'VAR'
    PROCEDURE = 'PROCEDURE'
    BEGIN = 'BEGIN'
    END = 'END'  # marks the end of the block
    # misc
    ID = 'ID'
    INTEGER_CONST = 'INTEGER_CONST'
    REAL_CONST = 'REAL_CONST'
    ASSIGN = ':='
    EOF = 'EOF'


def _build_reserved_keywords():  # 保留关键字
    """Build a dictionary of reserved keywords.

    The function relies on the fact that in the TokenType
    enumeration the beginning of the block of reserved keywords is
    marked with PROGRAM and the end of the block is marked with
    the END keyword.

    Result:
        {'PROGRAM': <TokenType.PROGRAM: 'PROGRAM'>,
         'INTEGER': <TokenType.INTEGER: 'INTEGER'>,
         'REAL': <TokenType.REAL: 'REAL'>,
         'DIV': <TokenType.TokenType.INTEGER_DIV: 'DIV'>,
         'VAR': <TokenType.VAR: 'VAR'>,
         'TokenType.PROCEDURE': <TokenType.TokenType.PROCEDURE: 'TokenType.PROCEDURE'>,
         'BEGIN': <TokenType.BEGIN: 'BEGIN'>,
         'END': <TokenType.END: 'END'>}
    """
    # enumerations support iteration, in definition order
    tt_list = list(TokenType)
    start_index = tt_list.index(TokenType.PROGRAM)
    end_index = tt_list.index(TokenType.END)
    reserved_keywords = {
        token_type.value: token_type
        for token_type in tt_list[start_index:end_index + 1]
    }
    return reserved_keywords


def sys_int():
    global RESERVED_KEYWORDS
    RESERVED_KEYWORDS = _build_reserved_keywords()
    print(RESERVED_KEYWORDS)


##############################################
#  Lexer   词法分析器                         #
##############################################

class Token:  # 定义记号类
    def __init__(self, type, value, lineno=None, column=None):  # 定义构造方法
        self.type = type  # 记号中值的类型
        self.value = value  # 记号中的值
        self.lineno = lineno
        self.column = column

    def __str__(self):
        """String representation of the class instance.

        Example:
            >>> Token(TokenType.INTEGER, 7, lineno=5, column=10)
            Token(TokenType.INTEGER, 7, position=5:10)
        """
        return 'Token({type}, {value}, position={lineno}:{column})'.format(
            type=self.type,
            value=repr(self.value),
            lineno=self.lineno,
            column=self.column,
        )

    def __repr__(self):  # 也可以写成 __repr__=__str__
        return self.__str__()


class Lexer(object):  # 词法分析器
    def __init__(self, text):  # 定义构造方法获取用户输入的表达式
        self.text = text  # 用户输入的表达式
        self.pos = 0  # 获取表达式中每一个字符时的位置
        self.current_char = self.text[self.pos]  # 设置当前字符为指定位置的字符
        # token line number and column number
        self.lineno = 1
        self.column = 1

    def error(self):  # 定义提示错误的方法
        s = "Lexer error on '{lexeme}' line: {lineno} column: {column}".format(
            lexeme=self.current_char,
            lineno=self.lineno,
            column=self.column,
        )
        raise LexerError(message=s)

    def advance(self):  # 定义获取下一个字符的方法
        """Advance the `pos` pointer and set the `current_char` variable."""
        if self.current_char == '\n':
            self.lineno += 1
            self.column = 0

        self.pos += 1  # 获取字符的位置自增
        if self.pos >= len(self.text):  # 如果位置到达字符串的末尾
            self.current_char = None  # 设置当前字符为None值
        else:  # 否则
            self.current_char = self.text[self.pos]  # 设置当前字符为指定位置的字符
            self.column += 1

    def skip_whitespace(self):  # 定义跳过空格的方法
        while self.current_char is not None and self.current_char.isspace():  # 如果当前字符不是None值并且当前字符是空格
            self.advance()  # 获取下一个字符

    def skip_comment(self):  # 添加跳过注释内容到的方法
        while self.current_char != '}':  # 如果当前字符不是注释结束符号
            self.advance()  # 提取下一个字符
        self.advance()  # 提取下一个字符（跳过注释结束符号）

    def peek(self):
        pos = self.pos + 1  # 获取下一个位置
        if pos >= len(self.text):  # 如果超出文本末端
            return None  # 返回None
        else:  # 否则
            return self.text[pos]  # 返回下一位置字符

    def _id(self):  # 获取保留字或赋值名称记号的方法
        result = ''
        while self.current_char is not None and self.current_char.isalnum():  # 如果当前字符是字母数字
            result += self.current_char  # 连接字符
            self.advance()  # 提取下一个字符
        # 如果是保留字返回保留字记号，否则返回ID记号;upper():使关键词支持不区分大小写
        keytype = RESERVED_KEYWORDS.get(result.upper())
        token = Token(TokenType.ID, result, self.lineno, self.column) if keytype is None else Token(keytype,
                                                                                                    result.upper(),
                                                                                                    self.lineno,
                                                                                                    self.column)
        return token

    def number(self):  # 获取数字
        result = ''
        while self.current_char is not None and (
                self.current_char.isdigit() or self.current_char == '.'):  # 如果当前字符不是None值并且当前字符是数字
            result += self.current_char  # 连接数字
            self.advance()  # 获取下一个字符
        if ('.' in result):
            return Token(TokenType.REAL_CONST, float(result), self.lineno, self.column)  # 返回浮点数
        else:
            return Token(TokenType.INTEGER_CONST, int(result), self.lineno, self.column)  # 返回整数

    def get_next_token(self):
        while self.current_char is not None:  # 如果当前字符不是None值
            if self.current_char.isspace():  # 如果当前字符是空格
                self.skip_whitespace()  # 跳过所有空格
                continue
            if self.current_char == '{':  # 注释
                self.skip_comment()  # 跳过所有注释
                continue
            if self.current_char.isdigit():  # 如果当前字符是整数
                return self.number()  # 获取完整的数字创建记号对象并返回
            if self.current_char.isalpha():  # 如果当前字符是字母
                return self._id()  # 调用方法返回保留字或赋值名称的记号
            if self.current_char == ':' and self.peek() == '=':  # 如果当前字符是“:”，并且下一个字符是“=”。
                self.advance()  # 提取下一个字符
                self.advance()  # 提取下一个字符
                return Token(TokenType.ASSIGN, ":=")  # 返回赋值符的记号

            try:
                # get enum member by value, e.g.
                # TokenType(';') --> TokenType.TokenType.SEMI
                token_type = TokenType(self.current_char)
            except ValueError:
                # no enum member with value equal to self.current_char
                self.error()
            else:
                # create a token with a single-character lexeme as its value
                token = Token(
                    type=token_type,
                    value=token_type.value,  # e.g. ';', '.', etc
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                return token

            self.error()  # 如果以上都不是，则抛出异常。
        return Token(TokenType.EOF, None)  # 遍历结束返回结束标识创建的记号对象


##############################################
#  Parser   语法分析器                        #
##############################################
class AST(object):
    pass


class Num(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class BinOp(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class UnaryOp(AST):
    def __init__(self, op, expr):
        self.token = self.op = op
        self.expr = expr


class Compound(AST):  # 添加复合语句节点
    def __init__(self):
        self.children = []  # 子节点列表


class Assign(AST):  # 添加赋值语句节点
    def __init__(self, left, operator, right):
        self.left = left  # 变量名称
        self.token = self.operator = operator  # 记号和赋值符号
        self.right = right  # 右侧表达式


class Variable(AST):  # 添加变量节点
    def __init__(self, token):
        self.token = token  # 记号
        self.name = token.value  # 变量值


class NoOp(AST):  # 添加空语句节点
    pass  # 无内容


class Type(AST):  # 定义类型节点
    def __init__(self, token):
        self.token = token
        self.name = token.value


class VarDecl(AST):  # 定义变量声明节点
    def __init__(self, var_node, type_node):  # 变量声明由变量和类型组成
        self.var_node = var_node
        self.type_node = type_node


class ProcedureDecl(AST):  # 添加过程声明节点
    def __init__(self, name, params, block_node):
        self.name = name  # 名称
        self.params = params  # 参数可能多个，因此是个列表
        self.block_node = block_node  # 块节点


class Param(AST):  # 过程（函数）参数节点
    def __init__(self, var_node, type_node):
        self.var_node = var_node  # 参数变量名称
        self.type_node = type_node  # 参数变量类型


class ProcedureCall(AST):  # 过程调用节点
    def __init__(self, proc_name, actual_params, token):
        self.proc_name = proc_name  # 过程名称
        self.actual_params = actual_params  # 过程实参列表
        self.token = token  # 过程token本身
        self.proc_symbol = None  # 过程符号（里面主要存放了参数列表等信息）


class Block(AST):  # 定义语句块节点
    def __init__(self, declarations, compound_statement):  # 语句块由声明和符合语句组成
        self.declarations = declarations
        self.compound_statement = compound_statement


class Program(AST):  # 定义程序节点
    def __init__(self, name, block):  # 程序由名称和语句块组成
        self.name = name
        self.block = block


class Parser:  # 语法分析器
    def __init__(self, lexer):  # 定义构造方法获取用户输入的表达式
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()  # 语法分析器初始化

    def error(self, error_code, token):
        raise ParserError(
            error_code=error_code,
            token=token,
            message=f'{error_code.value} -> {token}',
        )

    def eat(self, token_type):
        # print('current_token:',self.current_token)
        if (self.current_token.type == token_type):
            self.current_token = self.lexer.get_next_token()
        else:  # 否则
            self.error(
                error_code=ErrorCode.UNEXPECTED_TOKEN,
                token=self.current_token, )  # 抛出异常

    def factor(self):  # 语法分析器最底层结构：整数或括号
        token = self.current_token  # 获取记号
        if (token.type in (TokenType.PLUS, TokenType.MINUS)):
            self.eat(token.type)
            node = UnaryOp(token, self.factor())
            return node
        if (token.type in (TokenType.INTEGER_CONST, TokenType.REAL_CONST)):  # 整数
            self.eat(token.type)
            return Num(token)  # 返回数字节点对象
        elif (token.type == TokenType.LPAREN):  # 左括号
            self.eat(TokenType.LPAREN)
            node = self.expr()  # 求出括号里面的AST树
            self.eat(TokenType.RPAREN)  # 右括号
            return node  # 返回括号内的AST树
        else:  # 新增变量因子
            node = self.variable()  # 获取变量节点
            return node  # 返回变量节点

    def term(self):  # 语法分析器中间层结构：乘除
        node = self.factor()  # 获取第一个数字树,如没有乘除法，将直接返回一个代表数字的叶节点树
        while (self.current_token.type in (TokenType.MUL, TokenType.INTEGER_DIV, TokenType.FLOAT_DIV)):
            token = self.current_token
            self.eat(token.type)
            # 生成新的树：把目前已经获取到的乘除法树整体做为左子树，起到连续乘除的作用
            node = BinOp(left=node, op=token, right=self.factor())
            # 新的树以取得新的数字或括号内的树为右子树
        return node

    def expr(self):  # 语法分析器最高层结构：加减
        node = self.term()  # 获取第一段乘除
        while (self.current_token.type in (TokenType.PLUS, TokenType.MINUS)):
            token = self.current_token
            if token.type == TokenType.PLUS:
                self.eat(TokenType.PLUS)
            if token.type == TokenType.MINUS:
                self.eat(TokenType.MINUS)
            # 生成新的树：把目前已经获取到的加减法树整体做为左子树，起到连续加减的作用
            node = BinOp(left=node, op=token, right=self.term())
            # 新的树以取得新的数字或括号内的树为右子树
        return node

    def variable(self):  # 添加获取变量节点的方法
        node = Variable(self.current_token)  # 获取变量节点
        self.eat(TokenType.ID)  # 验证变量名称
        return node  # 返回变量节点

    def empty(self):  # 添加获取空语句节点的方法
        return NoOp()  # 返回空语句节点

    def assignment_statement(self):  # 添加获取赋值语句节点的方法
        left = self.variable()  # 获取变量名称节点
        token = self.current_token  # 获取当前记号
        self.eat(TokenType.ASSIGN)  # 验证赋值符
        right = self.expr()  # 获取表达式节点
        node = Assign(left, token, right)  # 组成赋值语句节点
        return node  # 返回赋值语句节点

    def statement(self):  # 添加获取语句节点的方法
        """
        statement : compound_statement   #复合语句
                  | proccall_statement   #过程调用语句
                  | assignment_statement #变量声明语句
                  | empty                #空
        """
        if self.current_token.type == TokenType.BEGIN:  # 如果遇到BEGIN，说明包含复合语句。
            node = self.compound_statement()  # 获取复合语句节点
        # 如果遇到一个名称和括号，说明是过程调用
        elif (self.current_token.type == TokenType.ID and self.lexer.current_char == '('):
            node = self.proccall_statement()  # 获取过程调用节点
        # 如果遇到一个名称，说明是赋值语句。
        elif self.current_token.type == TokenType.ID:
            node = self.assignment_statement()  # 获取赋值语句节点
        else:  # 否则就是空语句
            node = self.empty()  # 返回空语句节点
        return node  # 返回语句节点

    def statement_list(self):  # 添加获取语句列表节点的方法
        node = self.statement()  # 获取第一条语句节点
        nodes = [node]  # 添加第一条语句节点到列表
        while self.current_token.type == TokenType.SEMI:  # 如果遇到分号
            self.eat(TokenType.SEMI)  # 验证分号
            nodes.append(self.statement())  # 添加下一条语句节点到列表
        if self.current_token.type == TokenType.ID:  # 如果只遇到一个名称而非语句
            self.error()  # 抛出异常
        return nodes  # 返回语句节点列表

    def compound_statement(self):  # 添加获取复合语句节点的方法
        self.eat(TokenType.BEGIN)
        nodes = self.statement_list()  # 包含节点为语句列表
        self.eat(TokenType.END)

        root = Compound()  # 创建复合语句节点对象
        root.children = nodes  # 将语句节点列表添作为复合语句节点的子节点列表
        return root  # 返回复合语句节点对象

    def type_spec(self):  # 构造变量类型节点的方法
        token = self.current_token  # 获取当前记号
        if token.type == TokenType.INTEGER:  # 如果是整数类型
            self.eat(TokenType.INTEGER)  # 验证整数记号
        else:  # 否则
            self.eat(TokenType.REAL)  # 验证实数记号
        node = Type(token)  # 创建类型节点
        return node  # 返回类型节点

    def declarations(self):  # 构造声明节点的方法
        declarations = []  # 声明节点包含多个变量声明节点
        while True:  # 遍历声明
            if self.current_token.type == TokenType.VAR:  # 如果当前记号为变量
                self.eat(TokenType.VAR)  # 验证记号
                while self.current_token.type == TokenType.ID:  # 遍历变量名称
                    declarations.extend(self.variable_declaration())  # 声明列表中添加变量声明
                    self.eat(TokenType.SEMI)  # 验证分号
            elif self.current_token.type == TokenType.PROCEDURE:  # 当前记号类型是过程时
                self.eat(TokenType.PROCEDURE)  # 验证过程类型
                procedure_name = self.current_token.value  # 获取过程名称
                self.eat(TokenType.ID)  # 验证过程名称
                params = []  # 参数列表
                if self.current_token.type == TokenType.LPAREN:  # 如果遇到左括号
                    self.eat(TokenType.LPAREN)  # 验证左括号
                    params = self.formal_parameter_list()  # 获取参数列表
                    self.eat(TokenType.RPAREN)  # 验证右括号
                self.eat(TokenType.SEMI)  # 验证分号
                block_node = self.block()  # 获取过程中的块
                procedure_decl = ProcedureDecl(procedure_name, params, block_node)  # 创建包含参数的过程声明对象
                declarations.append(procedure_decl)  # 声明列表末尾添加新的过程声明
                self.eat(TokenType.SEMI)  # 验证分号
            else:  # 否则
                break  # 结束声明遍历
        return declarations

    def variable_declaration(self):  # 构造变量声明节点的方法
        var_nodes = [Variable(self.current_token)]  # 第一个变量声明节点添加到变量声明节点列表
        self.eat(TokenType.ID)  # 验证变量名称记号
        while self.current_token.type == TokenType.COMMA:  # 遍历逗号
            self.eat(TokenType.COMMA)  # 验证逗号
            var_nodes.append(Variable(self.current_token))  # 添加变量节点到变量节点列表
            self.eat(TokenType.ID)  # 验证变量名称记号
        self.eat(TokenType.COLON)  # 验证冒号
        type_node = self.type_spec()  # 一组变量声明的类型节点
        var_declarations = [VarDecl(var_node, type_node) for var_node in var_nodes]  # 生成变量声明列表
        return var_declarations  # 返回变量声明节点列表

    def formal_parameter_list(self):  # 添加创建参数列表节点的方法(循环完所有行参数)
        if self.current_token.type != TokenType.ID:  # 如果没有参数
            return []  # 返回空列表
        param_nodes = self.formal_parameters()  # 添加同类型参数节点到参数节点列表
        while self.current_token.type == TokenType.SEMI:  # 当前记号是分号时还有更多参数
            self.eat(TokenType.SEMI)  # 验证分号
            param_nodes.extend(self.formal_parameters())  # 添加更多参数节点到参数节点列表
        return param_nodes  # 返回参数节点列表

    def formal_parameters(self):  # 添加创建参数节点的方法  （循环完每一行参数）
        param_nodes = []  # 参数节点列表
        param_tokens = [self.current_token]  # 参数记号列表
        self.eat(TokenType.ID)  # 验证第一个参数名称
        while self.current_token.type == TokenType.COMMA:  # 当遇到逗号时
            self.eat(TokenType.COMMA)  # 验证逗号
            param_tokens.append(self.current_token)  # 添加记号到参数记号列表
            self.eat(TokenType.ID)  # 验证参数名称
        self.eat(TokenType.COLON)  # 遍历结束验证冒号
        type_node = self.type_spec()  # 获取参数类型节点
        for param_token in param_tokens:  # 遍历参数记号列表
            param_node = Param(Variable(param_token), type_node)  # 通过参数记号创建参数节点
            param_nodes.append(param_node)  # 添加参数节点到参数节点列表
        return param_nodes  # 返回参数节点列表

    def proccall_statement(self):
        """proccall_statement : ID LPAREN (expr (COMMA expr)*)? RPAREN"""
        token = self.current_token

        proc_name = self.current_token.value  # 解析出过程调用时使用的名称
        self.eat(TokenType.ID)  # 过程调用名称是一个ID类型的token
        self.eat(TokenType.LPAREN)  # 解析左括号
        actual_params = []  # 过程实参列表
        if self.current_token.type != TokenType.RPAREN:  # 不是右括号
            node = self.expr()  # 解析出过程调用时括号内的第1个实参expr节点
            actual_params.append(node)  # 第1个expr节点放入实参列表
        while self.current_token.type == TokenType.COMMA:  # 解析出第1个逗号后面的所有实参expr节点
            self.eat(TokenType.COMMA)
            node = self.expr()
            actual_params.append(node)  # 把全部逗号分隔的expr节点全部放入实参列表

        self.eat(TokenType.RPAREN)  # 右括号
        node = ProcedureCall(  # 最终生成过程调用节点：包括过程名称和实参列表
            proc_name=proc_name,
            actual_params=actual_params,
            token=token,
        )
        return node

    def visit_ProcedureCall(self, node):
        pass  # Pascal程序不支持过程调用的引用

    def block(self):  # 构造块节点的方法
        declarations = self.declarations()
        compound_statement = self.compound_statement()
        node = Block(declarations, compound_statement)  # 块节点由声明节点和符合语句节点组成
        return node

    def program(self):
        self.eat(TokenType.PROGRAM)  # 验证程序开始标记
        var_node = self.variable()  # 获取变量节点
        program_name = var_node.name  # 获取程序名称
        self.eat(TokenType.SEMI)  # 验证分号
        block_node = self.block()  # 获取块节点
        node = Program(program_name, block_node)  # 创建程序节点
        self.eat(TokenType.DOT)  # 验证程序结束符号
        return node  # 返回程序节点

    def parser(self):
        node = self.program()  # 获取程序所有节点
        if self.current_token.type != TokenType.EOF:  # 如果当前不是文件末端记号
            self.error()  # 抛出异常
        return node  # 返回程序节点


##############################################
#  NodeVisitor  AST树访问基类                 #
##############################################
class NodeVisitor(object):
    def visit(self, node):  # 节点遍历
        # 获取节点类型名称组成访问器方法名（子类Interpreter中方法的名称）
        method_name = 'visit_' + type(node).__name__
        # 获取访问器对象，找不到访问器时获取“generic_visit”
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception('No visit_{} method'.format(type(node).__name__))


##############################################
#  Semantic Analyzer  语义分析器              #
##############################################
class Symbol:  # 添加符号类
    def __init__(self, name, symbol_type=None):
        self.name = name  # 符号名称
        self.symbol_type = symbol_type  # 符号类型


class BuiltinTypeSymbol(Symbol):  # 添加内置类型符号类
    def __init__(self, name):
        super().__init__(name)  # 调用基类构造函数初始化

    def __str__(self):
        return self.name  # 返回符号名称

    def __repr__(self):
        return f"{self.__class__.__name__}(name='{self.name}')"  # 输出类名和符号名称


class VarSymbol(Symbol):  # 添加变量符号类
    def __init__(self, name, symbol_type):
        super().__init__(name, symbol_type)  # 调用基类构造函数初始化

    def __str__(self):
        return f"<{self.__class__.__name__}(name='{self.name}':type='{self.symbol_type}')>"  # 输出类名、符号名称和类型

    __repr__ = __str__


class ProcedureSymbol(Symbol):  # 添加过程符号类
    def __init__(self, name, formal_params=None):  # 过程包含名称与形式参数信息
        super().__init__(name)
        self.formal_params = formal_params if formal_params is not None else []  # 获取形式参数，如果未传入则为空列表。
        # 该过程下面的程序体(body)节点Block (AST子树)
        self.block_ast = None

    def __str__(self):
        return f"<{self.__class__.__name__}(name='{self.name}',parameters={self.formal_params})>"  # 过程的信息

    __repr__ = __str__


class ScopedSymbolTable:  # 添加符号表类
    def __init__(self, scope_name, scope_level, enclosing_scope=None):
        self._symbols = OrderedDict()  # 存储符号的有序字典
        self.scope_name = scope_name  # 添加作用域名称
        self.scope_level = scope_level  # 添加作用域级别
        self.enclosing_scope = enclosing_scope  # 添加外围作用域

    def _init_builtins(self):  # 定义初始化内置类型的方法
        self.insert(BuiltinTypeSymbol('INTEGER'))  # 通过insert()方法存入内置类型符号
        self.insert(BuiltinTypeSymbol('REAL'))  # 通过insert()方法存入内置类型符号

    def __str__(self):
        scope_header = '作用域符号表：'
        lines = ['\n', scope_header, '=' * len(scope_header) * 2]
        for header_name, header_value in (
                ('作用域名称', self.scope_name),
                ('作用域级别', self.scope_level),
                ('外围作用域', self.enclosing_scope.scope_name if self.enclosing_scope else None)  # 如果不存在外围作用域则为None
        ):  # 遍历作用域名称和级别以及外围作用域
            lines.append(f'{header_name:15}:{header_value}')
        symtab_header = '符号表中的内容：'
        lines.extend(['\n', symtab_header, '-' * len(symtab_header) * 2])
        lines.extend([f'{key:8}: {value}' for key, value in self._symbols.items()])
        s = '\n'.join(lines)
        return s

    __repr__ = __str__

    def insert(self, symbol):  # 添加存入符号的方法
        print(f'存入：{symbol}')
        self._symbols[symbol.name] = symbol  # 以符号名称为键存入符号

    def lookup(self, name, current_scope_only=False):  # 符号表类的查找方法
        print(f'查询：{name}(作用域：{self.scope_name})')
        symbol = self._symbols.get(name)  # 在当前作用域查找符号
        if symbol:  # 如果找到符号
            return symbol  # 返回符号
        if current_scope_only:  # 如果仅查找当前作用域，在没有查到符号时返回None。
            return None
        if self.enclosing_scope:  # 如果当前作用域没有找到符号并且存在外围作用域
            return self.enclosing_scope.lookup(name)  # 递归方式在外围作用域进行查找


class SemanticAnalyzer(NodeVisitor):  # 添加语义分析器
    def __init__(self):
        self.current_scope = None  # 初始化保存符号表的变量

    def error(self, error_code, token):
        raise SemanticError(
            error_code=error_code,
            token=token,
            message=f'{error_code.value} -> {token}',
        )

    def visit_Program(self, node):  # 添加与访问变量声明相关的访问方法
        print('>>> 进入作用域：global')
        global_scope = ScopedSymbolTable(scope_name='global', scope_level=1,
                                         enclosing_scope=self.current_scope)  # 创建全局作用域符号表
        global_scope._init_builtins()  # 初始化内置类型
        self.current_scope = global_scope
        self.visit(node.block)
        print(global_scope)

        self.current_scope = self.current_scope.enclosing_scope  # 离开作用域时设置当前作用域为外围作用域
        print('<<< 离开作用域：global')

    def visit_ProcedureDecl(self, node):
        proc_name = node.name  # 获取过程节点名称
        proc_symbol = ProcedureSymbol(proc_name)  # 创建过程符号
        self.current_scope.insert(proc_symbol)  # 过程符号添加到当前作用域
        print(f'>>> 进入作用域：{proc_name}')  # 显示输出进入过程作用域
        proc_scope = ScopedSymbolTable(scope_name=proc_name, scope_level=self.current_scope.scope_level + 1,
                                       enclosing_scope=self.current_scope)  # 创建过程的作用域符号表
        self.current_scope = proc_scope  # 当前作用域设置为过程作用域
        for param in node.params:  # 遍历过程的形式参数列表
            param_type = self.current_scope.lookup(param.type_node.name)  # 获取参数类型
            param_name = param.var_node.name  # 获取参数名称
            param_symbol = VarSymbol(param_name, param_type)  # 创建参数符号
            self.current_scope.insert(param_symbol)  # 将参数符号添加到当前作用域
            proc_symbol.formal_params.append(param_symbol)  # 为过程符号参数列表添加参数符号
        self.visit(node.block_node)  # 访问过程中的块节点
        print(proc_scope)  # 显示输出过程作用域符号表信息
        self.current_scope = self.current_scope.enclosing_scope  # 离开作用域时设置当前作用域为外围作用域
        print(f'<<< 离开作用域：{proc_name}')  # 显示输出离开过程作用域

        # 把过程声明下面的Block节点作为过程符号的block_ast成员。在过程调用的时候，解释器需要访问它
        proc_symbol.block_ast = node.block_node

    def visit_Block(self, node):  # 添加与访问变量声明相关的访问方法
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_VarDecl(self, node):  # 添加访问变量声明节点方法
        symbol_type = node.type_node.name  # 获取变量类型名称
        self.current_scope.lookup(symbol_type)
        var_name = node.var_node.name
        var_symbol = VarSymbol(var_name, symbol_type)
        # Signal an error if the table already has a symbol
        # with the same name
        if self.current_scope.lookup(var_name, current_scope_only=True):
            self.error(
                error_code=ErrorCode.DUPLICATE_ID,
                token=node.var_node.token,
            )
        self.current_scope.insert(var_symbol)  # 变量符号对象添加到符号表

    def visit_Compound(self, node):  # 添加与访问变量声明相关的访问方法
        for child in node.children:
            self.visit(child)

    def visit_BinOp(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_Assign(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_Variable(self, node):
        var_name = node.name
        var_symbol = self.current_scope.lookup(var_name)
        if var_symbol is None:
            self.error(error_code=ErrorCode.ID_NOT_FOUND, token=node.token)

    def visit_Type(self, node):
        pass

    def visit_Num(self, node):
        pass

    def visit_NoOp(self, node):  # 添加与访问变量声明相关的访问方法
        pass

    def visit_UnaryOp(self, node):
        pass

    def visit_ProcedureCall(self, node):  # 过程调用遍历方法
        for param_node in node.actual_params:  # 遍历调用所有的实参即可
            self.visit(param_node)
        proc_name = node.proc_name
        # 从当前域中查询到过程符号
        proc_symbol = self.current_scope.lookup(proc_name)
        # 把查询到的过程符号放到当前过程调用节点下，在后面的解释器interpreter执行过程调用的时候要用到它
        node.proc_symbol = proc_symbol


##############################################
#  Interpreter  解释器                        #
##############################################
class CallStack:
    def __init__(self):
        self._records = []

    def push(self, ar):
        self._records.append(ar)

    def pop(self):
        return self._records.pop()

    def peek(self):
        return self._records[-1]

    def __str__(self):
        s = '\n'.join(repr(ar) for ar in reversed(self._records))
        s = f'现在调用栈CALL STACK里的内容:\n{s}\n'
        return s

    def __repr__(self):
        return self.__str__()


class ARType(Enum):
    PROGRAM = 'PROGRAM'
    PROCEDURE = 'PROCEDURE'


class ActivationRecord:
    def __init__(self, name, type, nesting_level):
        self.name = name
        self.type = type
        self.nesting_level = nesting_level
        self.members = {}

    def __setitem__(self, key, value):
        self.members[key] = value

    def __getitem__(self, key):
        return self.members[key]

    def get(self, key):
        return self.members.get(key)

    def __str__(self):
        lines = [
            '{level}: {type} {name}'.format(
                level=self.nesting_level,
                type=self.type.value,
                name=self.name,
            )
        ]
        for name, val in self.members.items():
            lines.append(f'   {name:<20}: {val}')

        s = '\n'.join(lines)
        return s

    def __repr__(self):
        return self.__str__()


class Interpreter(NodeVisitor):
    def __init__(self, tree):  # 修改构造方法
        self.tree = tree  # 获取参数AST
        self.call_stack = CallStack()  # 创建调用栈

    def log(self, msg):
        print(msg)

    #   以下为AST各节点的遍历方法--------------------
    def visit_BinOp(self, node):  # 访问二元运算符类型节点的方法
        if node.op.type == TokenType.PLUS:  # 如果操作符类型是加法
            # 分别访问左侧和右侧的记号，将获取的值进行加法运算，并返回结果。
            return self.visit(node.left) + self.visit(node.right)
        elif node.op.type == TokenType.MINUS:
            return self.visit(node.left) - self.visit(node.right)
        elif node.op.type == TokenType.MUL:
            return self.visit(node.left) * self.visit(node.right)
        elif node.op.type == TokenType.INTEGER_DIV:
            return self.visit(node.left) // self.visit(node.right)
        elif node.op.type == TokenType.FLOAT_DIV:
            return self.visit(node.left) / self.visit(node.right)

    def visit_Num(self, node):  # 访问数字类型节点的方法
        return node.value

    def visit_UnaryOp(self, node):  # 一元运算符类型节点的方法
        op = node.op.type
        if op == TokenType.PLUS:
            return +self.visit(node.expr)
        elif op == TokenType.MINUS:
            return -self.visit(node.expr)

    def visit_Compound(self, node):  # 访问复合语句节点
        for child in node.children:  # 遍历复合语句节点的子节点
            self.visit(child)  # 访问子节点

    def visit_Assign(self, node):  # 访问赋值语句节点
        var_name = node.left.name  # 获取变量名称
        var_value = self.visit(node.right)  # 获取赋值语句右边表达式的值
        ar = self.call_stack.peek()  # 获取调用栈的栈顶
        # 以变量名称为键添加变量值到栈顶活动记录ActivationRecord的members字典
        ar[var_name] = var_value

    def visit_Variable(self, node):  # 访问变量节点
        var_name = node.name  # 获取变量名称
        ar = self.call_stack.peek()  # 获取调用栈的栈顶
        # 从栈顶活动记录ActivationRecord的members字典获取变量值（变量名称为键）
        var_value = ar.get(var_name)
        return var_value

    def visit_NoOp(self, node):  # 访问空语句节点
        pass  # 无操作

    def visit_Program(self, node):  # 添加访问程序的方法
        self.log(str(self.call_stack))  # 打印调用栈,应为空

        # 进入程序PROGRAM
        program_name = node.name
        self.log(f'>>>进入程序: PROGRAM {program_name}')
        # 创建当前活动记录PROGRAM
        ar = ActivationRecord(
            name=program_name,
            type=ARType.PROGRAM,
            nesting_level=1,
        )

        self.call_stack.push(ar)  # 当前活动记录入栈
        self.visit(node.block)  # 访问PROGRAM下的语句块Block节点
        self.log(f'<<<离开程序: PROGRAM {program_name}')  # 离开当前活动记录
        self.call_stack.pop()  # 当前活动记录出栈
        self.log(str(self.call_stack))  # 打印调用栈

    def visit_ProcedureCall(self, node):
        self.log(str(self.call_stack))  # 打印调用栈

        # 进入过程调用PROCEDURE
        proc_name = node.proc_name
        self.log(f'>>>>>>进入过程调用: PROCEDURE {proc_name}')
        # 创建当前活动记录PROCEDURE
        ar = ActivationRecord(
            name=proc_name,
            type=ARType.PROCEDURE,
            nesting_level=2,
        )

        proc_symbol = node.proc_symbol  # 取到当前节点的过程符号
        formal_params = proc_symbol.formal_params  # 取到形式参数列表
        actual_params = node.actual_params  # 取到实际参数列表
        # 把实际参数的值存入到对应的形式参数里
        for param_symbol, argument_node in zip(formal_params, actual_params):
            ar[param_symbol.name] = self.visit(argument_node)
        self.log(f'>>>>>>实参传递值到形参<<<<<<')

        self.call_stack.push(ar)  # 当前活动记录入栈
        self.log(str(self.call_stack))  # 打印调用栈

        self.log(f'>>>>>>执行过程调用<<<<<<')
        self.visit(proc_symbol.block_ast)  # 访问PROCEDURE下的语句块Block节点
        self.log(str(self.call_stack))  # 打印调用栈

        self.log(f'<<<<<<离开过程调用: PROCEDURE {proc_name}')  # 离开当前活动记录

        self.call_stack.pop()  # 当前活动记录出栈
        self.log(str(self.call_stack))  # 打印调用栈

    def visit_Block(self, node):  # 添加访问语句块的方法
        for declaration in node.declarations:  # 遍历声明列表
            self.visit(declaration)  # 访问声明
        self.visit(node.compound_statement)  # 访问复合语句

    def visit_VarDecl(self, node):  # 添加访问变量声明的方法
        pass  # 无需处理

    def visit_ProcedureDecl(self, node):  # 添加访问过程声明的方法
        pass  # 暂不处理

    def visit_Type(self, node):  # 添加访问类型的方法
        pass  # 无需处理

    #  以上为AST各节点的遍历方法--------------------

    def interpret(self):  # 执行解释的方法
        tree = self.tree  # 获取AST
        if tree is None:  # 如果AST不存在
            return ''  # 返回空字符串
        return self.visit(tree)  # 否则访问AST


##############################################
#  DrawASTtree  AST树绘图器                   #
##############################################
class DrawASTtreeImg(NodeVisitor):  # 添加符号表生成器类
    def __init__(self, tree):  # 修改构造方法
        self.tree = tree  # 获取参数AST
        self.ASTdata = []  # 创建AST可视化数据

    #  以下为AST各节点的可视化遍历方法-------------------
    def visit_Program(self, node):
        blockdata = self.visit(node.block)
        Programdata = [{"children": blockdata, "name": "Program\n" + node.name}]
        return Programdata

    def visit_Block(self, node):
        decldata = []
        for declaration in node.declarations:
            decldata = decldata + self.visit(declaration)
        compdata = self.visit(node.compound_statement)
        declandcompdata = decldata + compdata  # Block节点由变量声明和复合语句组成
        Blockdata = [{"children": declandcompdata, "name": "Block"}]
        return Blockdata

    def visit_VarDecl(self, node):
        vardata = self.visit(node.var_node)
        Typedata = self.visit(node.type_node)
        VarDecldata = [{"children": vardata + Typedata, "name": "VarDecl"}]
        return VarDecldata

    def visit_ProcedureDecl(self, node):  # 定义过程声明的绘图方法
        Paramdata = []
        for paramnode in node.params:  # 遍历参数列表中的所有参数节点
            Paramdata = Paramdata + self.visit(paramnode)
        Blockdata = self.visit(node.block_node)  # Block节点
        ProcedureDecldata = [{"children": Paramdata + Blockdata, "name": "ProcDecl\n" + node.name}]
        # 过程声明节点由参数节点和Block节点组成
        return ProcedureDecldata

    def visit_Param(self, node):  # 过程参数节点的绘图方法
        vardata = self.visit_Variable(node.var_node)
        Typedata = self.visit_Type(node.type_node)
        Paramdata = [{"children": vardata + Typedata, "name": "Param"}]
        return Paramdata

    def visit_Variable(self, node):  # 访问变量节点
        var_name = node.name  # 获取变量名称
        vardata = [{"name": "Var\n" + var_name}]
        return vardata

    def visit_Type(self, node):  # 添加访问类型的方法
        type_name = node.name  # 获取变量类型名称
        Typedata = [{"name": "Type\n" + type_name}]
        return Typedata

    def visit_Compound(self, node):  # 访问复合语句节点
        comchild = []
        for child in node.children:  # 遍历复合语句节点的子节点
            comchild = comchild + self.visit(child)  # 访问子节点
        Compounddata = [{"children": comchild, "name": "Compound"}]
        return Compounddata

    def visit_Assign(self, node):  # 访问赋值语句节点
        vardata = {"name": "Var\n" + node.left.name}
        rightdata = self.visit(node.right)
        Assignchild = [vardata] + rightdata
        Assigndata = [{"children": Assignchild, "name": "Assign\n" + node.token.value}]
        return Assigndata

    def visit_BinOp(self, node):  # 访问二元运算符类型节点的方法
        leftdata = self.visit(node.left)
        rightdata = self.visit(node.right)
        BinOpdata = [{"children": leftdata + rightdata, "name": "BinOp\n" + node.op.type.value}]
        return BinOpdata

    def visit_Num(self, node):  # 访问数字类型节点的方法
        Numdata = [{"name": "Num\n" + str(node.value)}]
        return Numdata

    def visit_UnaryOp(self, node):  # 一元操作符
        op = node.op.type
        exprdata = self.visit(node.expr)
        UnaryOpdata = [{"children": exprdata, "name": "UnaryOp\n" + op}]
        return UnaryOpdata

    def visit_NoOp(self, node):  # 访问空语句节点
        NoOpdata = [{"name": "NoOp"}]
        return NoOpdata

    def visit_ProcedureCall(self, node):  # 访问过程调用节点
        paramdata = []
        # 所有实参节点都是过程调用节点的孩子
        for param_node in node.actual_params:
            paramdata = paramdata + (self.visit(param_node))
        ProcedureCall = [{"children": paramdata, "name": "ProcCall\n" + node.proc_name}]
        return ProcedureCall

    #  以上为AST各节点的可视化遍历方法-------------------

    def drawastimg(self):  # 绘制AST语法树图形
        tree = self.tree
        self.ASTdata = self.visit(tree)  # 返回语法树可视化的遍历结果
        Asttree = (
            Tree()
            .add("", data=self.ASTdata, initial_tree_depth=-1, orient="TB")
            .set_global_opts(title_opts=opts.TitleOpts(title="AST树结构"))
        )
        Asttree.render(r'AstTree.html')  # 绘制AST语法树图形


def main():
    # text = open('test.pas', 'r').read()
    text = '''
program Main;

procedure Alpha(a : integer; b : integer);
var x : integer;
begin
   x := (a + b ) * 2;
end;

begin { Main }

   Alpha(3 + 5, 7);  { procedure call }

end.  { Main }
    '''
    print('1.初始化Token类型和保留关键字...')
    sys_int()

    print('\n2.开始词法分析和语法分析生成AST语法树...')
    lexer = Lexer(text)
    parser = Parser(lexer)
    tree = parser.parser()
    print(tree)

    print('\n3.开始绘制AST树图...')
    drawastimg = DrawASTtreeImg(tree)
    drawastimg.drawastimg()
    print('AST语法树图:../AstTree.html')
    print('AST语法树的内容:', drawastimg.ASTdata)

    print('\n4.开始语义分析...')
    semantic_analyzer = SemanticAnalyzer()
    semantic_analyzer.visit(tree)

    print('\n5.开始解释代码...')
    interpreter = Interpreter(tree)
    interpreter.interpret()
    # print('全局存储中的内容：')
    # for k, v in interpreter.GLOBAL_MEMORY.items():
    #     print(f'{k}：{v}')


if __name__ == '__main__':
    main()
