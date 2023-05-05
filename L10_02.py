# 编写简单解释器进阶程序代码

# 2021-06-10
# 自己写的：
# 支持一位数加法:7+8
# 增加eat方法，让代码结构更好(向鲁斯兰的方法靠拢)
# 支持一位数减法:8-3、
# 支持两个操作数字之间空格如：9   +    3
# 支持任意位数字长度如: 8888  -  222
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

# 自己写的：支持程序头、变量申明和程序体等完整程序结构;支持浮点数;支持注释
# Myversion  2021-07-07

# 自己写的：做进一步优化,程序头区和变量声明区也全部装进AST树
# Myversion  2021-07-08

###############################################################################
#  LEXER     词法分析器                                                        #                                                                      #
###############################################################################

NUMBER, PLUS, MINUS, MUL, DIV, LPAREN, RPAREN, EOF = 'NUMBER', 'PLUS', 'MINUS', 'MUL', 'DIV', 'LPAREN', 'RPAREN', 'EOF'
# 整数，加法，减法，乘法、除法，左括号,右括号,结束标识

ASSIGN, SEMI, DOT, BEGIN, END, ID = 'ASSIGN', 'SEMI', 'DOT', 'BEGIN', 'END', 'ID'  # 增加新的终结符常量

PROGRAM, VAR, COMMA, COLON, INTEGER, REAL, DATATYPE = 'PROGRAM', 'VAR', 'COMMA', 'COLON', 'INTEGER', 'REAL', 'DATATYPE'


# ------词法分析器Lexer部分------
class Token:  # 定义记号类
    def __init__(self, type, value):  # 定义构造方法
        self.type = type  # 记号中值的类型
        self.value = value  # 记号中的值

    def __str__(self):  # 重写查看记号内容的方法
        return 'Token({type},{value})'.format(type=self.type, value=self.value)

    def __repr__(self):  # 也可以写成 __repr__=__str__
        return self.__str__()


RESERVED_KEYWORDS = {  # 保留字字典
    'BEGIN': Token(BEGIN, 'BEGIN'),
    'END': Token(END, 'END'),
    'DIV': Token(DIV, 'DIV'),
    'PROGRAM': Token(PROGRAM, 'PROGRAM'),  # 程序头区
    'VAR': Token(VAR, 'VAR'),  # 变量声明区
    'INTEGER': Token(DATATYPE, 'int'),  # 数据类型枚举
    'REAL': Token(DATATYPE, 'float')

}


class Lexer():  # 词法分析器
    def __init__(self, text):  # 定义构造方法获取用户输入的表达式
        self.text = text  # 用户输入的表达式
        self.position = 0  # 获取表达式中每一个字符时的位置
        self.current_char = self.text[self.position]  # 设置当前字符为指定位置的字符

    def error(self):  # 定义提示错误的方法
        raise Exception('警告：错误的输入内容！')  # 抛出异常

    def advance(self):  # 定义获取下一个字符的方法
        self.position += 1  # 获取字符的位置自增
        if self.position >= len(self.text):  # 如果位置到达字符串的末尾
            self.current_char = None  # 设置当前字符为None值
        else:  # 否则
            self.current_char = self.text[self.position]  # 设置当前字符为指定位置的字符

    def skip_whitespace(self):  # 定义跳过空格的方法
        while self.current_char is not None and self.current_char.isspace():  # 如果当前字符不是None值并且当前字符是空格
            self.advance()  # 获取下一个字符

    def skip_comment(self):  # 添加跳过注释内容到的方法
        while self.current_char != '}':  # 如果当前字符不是注释结束符号
            self.advance()  # 提取下一个字符
        self.advance()  # 提取下一个字符（跳过注释结束符号）

    def peek(self):
        pos = self.position + 1  # 获取下一个位置
        if pos >= len(self.text):  # 如果超出文本末端
            return None  # 返回None
        else:  # 否则
            return self.text[pos]  # 返回下一位置字符

    def _id(self):  # 获取保留字或赋值名称记号的方法
        result = ''
        while self.current_char is not None and (
                self.current_char.isalnum() or self.current_char == '_'):  # 如果当前字符是字母数字
            result += self.current_char  # 连接字符
            self.advance()  # 提取下一个字符
        token = RESERVED_KEYWORDS.get(result.upper(), Token('ID', result.upper()))  # 如果是保留字返回保留字记号，默认返回ID记号
        return token

    def number(self):  # 获取数字
        result = ''
        while self.current_char is not None and (
                self.current_char.isdigit() or self.current_char == '.'):  # 如果当前字符不是None值并且当前字符是数字
            result += self.current_char  # 连接数字
            self.advance()  # 获取下一个字符
        if ('.' in result):
            return float(result)  # 返回浮点数
        else:
            return int(result)  # 返回整数

    def get_next_token(self):
        while self.current_char is not None:  # 如果当前字符不是None值
            if self.current_char.isspace():  # 如果当前字符是空格
                self.skip_whitespace()  # 跳过所有空格
                continue
            if self.current_char == '{':  # 注释
                self.skip_comment()  # 跳过所有注释
                continue
            if self.current_char.isdigit():  # 如果当前字符是整数
                return Token(NUMBER, self.number())  # 获取完整的数字创建记号对象并返回
            if self.current_char == '+':  # 如果当前字符是加号
                self.advance()  # 跳到下一字符
                return Token(PLUS, '+')  # 创建记号对象并返回
            if self.current_char == '-':  # 如果当前字符是减号
                self.advance()  # 跳到下一字符
                return Token(MINUS, '-')  # 创建记号对象并返回
            if self.current_char == '*':
                self.advance()  # 跳到下一字符
                return Token(MUL, '*')  # 创建记号对象并返回
            if self.current_char == '/':
                self.advance()  # 跳到下一字符
                return Token(DIV, '/')  # 创建记号对象并返回
            if self.current_char == '(':
                self.advance()  # 跳到下一字符
                return Token(LPAREN, '(')  # 创建记号对象并返回
            if self.current_char == ')':
                self.advance()  # 跳到下一字符
                return Token(RPAREN, ')')  # 创建记号对象并返回
            if (self.current_char.isalpha() or self.current_char == '_'):  # 如果当前字符是字母
                return self._id()  # 调用方法返回保留字或赋值名称的记号
            if self.current_char == ':' and self.peek() == '=':  # 如果当前字符是“:”，并且下一个字符是“=”。
                self.advance()  # 提取下一个字符
                self.advance()  # 提取下一个字符
                return Token(ASSIGN, ':=')  # 返回赋值符的记号
            if self.current_char == ';':  # 如果当前字符是分号
                self.advance()  # 提取下一个字符
                return Token(SEMI, ';')  # 返回分号记号
            if self.current_char == '.':  # 如果当前字符是点
                self.advance()  # 提取下一个字符
                return Token(DOT, '.')  # 返回点记号
            if self.current_char == ':':  # 如果当前字符是冒号
                self.advance()  # 提取下一个字符
                return Token(COLON, ':')  # 返回冒号记号
            if self.current_char == ',':  # 如果当前字符是逗号
                self.advance()  # 提取下一个字符
                return Token(COMMA, ',')  # 返回逗号记号
            self.error()  # 如果以上都不是，则抛出异常。
        return Token(EOF, None)  # 遍历结束返回结束标识创建的记号对象


###############################################################################                                                                         #
#  PARSER   语法分析器                                                         #
###############################################################################

class AST(object):
    pass


class Num(AST):  # 数字类节点
    def __init__(self, token):
        self.token = token
        self.value = token.value


class BinOp(AST):  # 二元操作符类节点
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class UnaryOp(AST):  # 一元操作符类节点
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


class Vartype(AST):  # 变量类型节点
    def __init__(self, token, name):
        self.token = token  # 记号
        self.type = token.value  # 变量数据类型
        self.name = name  # 变量名称


class Parser:  # 语法分析器
    def __init__(self, lexer):  # 定义构造方法获取用户输入的表达式
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()  # 语法分析器初始化

    def error(self):  # 定义提示错误的方法
        raise Exception('警告：语法分析器出现错误！')  # 抛出异常

    def eat(self, token_type):
        print('current_token:', self.current_token)
        if (self.current_token.type == token_type):
            self.current_token = self.lexer.get_next_token()
        else:  # 否则
            self.error()  # 抛出异常

    def variable(self):  # 添加获取变量节点的方法
        node = Variable(self.current_token)  # 获取变量节点
        self.eat(ID)  # 验证变量名称
        return node  # 返回变量节点

    def empty(self):  # 添加获取空语句节点的方法
        return NoOp()  # 返回空语句节点

    def assignment_statement(self):  # 添加获取赋值语句节点的方法
        left = self.variable()  # 获取变量名称节点
        token = self.current_token  # 获取当前记号
        self.eat(ASSIGN)  # 验证赋值符
        right = self.expr()  # 获取表达式节点
        node = Assign(left, token, right)  # 组成赋值语句节点
        return node  # 返回赋值语句节点

    def statement(self):  # 添加获取语句节点的方法
        if self.current_token.type == BEGIN:  # 如果遇到BEGIN，说明包含复合语句。
            node = self.compound_statement()  # 获取复合语句节点
        elif self.current_token.type == ID:  # 如果遇到一个名称，说明是赋值语句。
            node = self.assignment_statement()  # 获取赋值语句节点
        else:  # 否则就是空语句
            node = self.empty()  # 返回空语句节点
        return node  # 返回语句节点

    def statement_list(self):  # 添加获取语句列表节点的方法
        node = self.statement()  # 获取第一条语句节点
        nodes = [node]  # 添加第一条语句节点到列表
        while self.current_token.type == SEMI:  # 如果遇到分号
            self.eat(SEMI)  # 验证分号
            nodes.append(self.statement())  # 添加下一条语句节点到列表
        if self.current_token.type == ID:  # 如果只遇到一个名称而非语句
            self.error()  # 抛出异常
        return nodes  # 返回语句节点列表

    def compound_statement(self):  # 添加获取复合语句节点的方法
        self.eat(BEGIN)
        nodes = self.statement_list()  # 包含节点为语句列表
        self.eat(END)

        root = Compound()  # 创建复合语句节点对象
        root.children = nodes  # 将语句节点列表添作为复合语句节点的子节点列表
        return root  # 返回复合语句节点对象

    def var_statement(self):  # 变量声明区节点生成方法
        self.eat(VAR)
        nodes = self.var_list()  # 获取所有的变量节点对象列表
        root = Compound()  # 创建变量声明区节点对象
        root.children = nodes
        return root  # 返回变量声明区节点对象

    def varline(self):  # 变量声明区每一行
        varname = Variable(self.current_token).name  # 把变量名称先暂时存起来
        varnames = [varname]  # 变量节点列表
        self.eat(ID)  # 验证变量名称
        while (self.current_token.type == COMMA):  # 遍历完所有逗号间隔的每个变量定义节点
            self.eat(COMMA)  # 验证逗号
            varname = Variable(self.current_token).name  # 获取变量节点
            varnames.append(varname)  # 累计所有的变量
            self.eat(ID)  # 验证变量名称
        self.eat(COLON)  # 验证冒号

        vartypetoken = self.current_token  # 获取数据类型（INTEGER OR REAL）
        # 把所有的变量名称组装成变量类型节点，最终汇总到数组
        nodes = [Vartype(vartypetoken, varname) for varname in varnames]

        self.eat(DATATYPE)  # 验证数据类型关键字
        self.eat(SEMI)  # 验证分号
        return nodes

    def var_list(self):  # 变量声明区
        nodes = []
        while (self.current_token.type != BEGIN):  # 保证读完变量区所有的行
            nodes = nodes + self.varline()  # 每一行变量解析
        return nodes  # 最终返回所有的变量类型节点

    def header(self):  # 程序头区，忽略其它信息，返回空列表
        nodes = []
        self.eat(PROGRAM)  # 程序头关键字
        self.eat(ID)  # 程序名称Part10
        self.eat(SEMI)  # 分号
        return nodes

    def header_statement(self):
        nodes = self.header()  # 空列表
        root = Compound()  # 创建程序头节点对象
        root.children = nodes  # 将程序头节点对象列表添作为程序头节点对象子节点
        return root  # 返回程序头节点对象

    def program(self):  # 全部程序解析
        nodes_header = [self.header_statement()]  # 把程序头区节点对象变成列表（暂时为空）
        nodes_vardec = [self.var_statement()]  # 把变量声明区节点对象变成列表
        nodes_com = [self.compound_statement()]  # 把程序体区的复合语句节点对象变成列表
        nodes = nodes_header + nodes_vardec + nodes_com  # 组装成一个总列表
        root = Compound()  # 最顶层的根节点
        root.children = nodes  # 把总列表装到根节点下面
        self.eat(DOT)  # 程序结束
        return root

    def factor(self):  # 语法分析器最底层结构：整数或括号
        token = self.current_token  # 获取记号
        if (token.type in (PLUS, MINUS)):
            self.eat(token.type)
            node = UnaryOp(token, self.factor())
            return node
        if (token.type == NUMBER):  # 整数
            self.eat(NUMBER)
            return Num(token)  # 返回数字节点对象
        elif (token.type == LPAREN):  # 左括号
            self.eat(LPAREN)
            node = self.expr()  # 求出括号里面的AST树
            self.eat(RPAREN)  # 右括号
            return node  # 返回括号内的AST树
        else:  # 新增变量因子
            node = self.variable()  # 获取变量节点
            return node  # 返回变量节点

    def term(self):  # 语法分析器中间层结构：乘除
        node = self.factor()  # 获取第一个数字树,如没有乘除法，将直接返回一个代表数字的叶节点树
        while (self.current_token.type in (MUL, DIV)):
            token = self.current_token
            if token.type == MUL:
                self.eat(MUL)
            if token.type == DIV:
                self.eat(DIV)
            # 生成新的树：把目前已经获取到的乘除法树整体做为左子树，起到连续乘除的作用
            node = BinOp(left=node, op=token, right=self.factor())
            # 新的树以取得新的数字或括号内的树为右子树
        return node

    def expr(self):  # 语法分析器最高层结构：加减
        node = self.term()  # 获取第一段乘除
        while (self.current_token.type in (PLUS, MINUS)):
            token = self.current_token
            if token.type == PLUS:
                self.eat(PLUS)
            if token.type == MINUS:
                self.eat(MINUS)
            # 生成新的树：把目前已经获取到的加减法树整体做为左子树，起到连续加减的作用
            node = BinOp(left=node, op=token, right=self.term())
            # 新的树以取得新的数字或括号内的树为右子树
        return node

    def parse(self):
        node = self.program()  # 获取程序所有节点
        if self.current_token.type != EOF:  # 如果当前不是文件末端记号
            self.error()  # 抛出异常
        return node  # 返回程序节点


###############################################################################
#  INTERPRETER    解释器
###############################################################################
class NodeVisitor(object):
    def visit(self, node):
        # 获取节点类型名称组成访问器方法名（子类Interpreter中方法的名称）
        method_name = 'visit_' + type(node).__name__
        # 获取访问器对象，找不到访问器时获取“generic_visit”
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception('No visit_{} method'.format(type(node).__name__))


class Interpreter(NodeVisitor):
    GLOBAL_SCOPE = {}  # 创建符号表
    VAR_TYPE = {}

    def __init__(self, parser):  # 定义构造方法获取用户输入的表达式
        self.parser = parser

    def error(self, varname):  # 定义提示错误的方法
        raise Exception('错误：变量类型不匹配', varname)  # 抛出异常

    def visit_BinOp(self, node):  # 访问二元运算符类型节点的方法
        if node.op.type == PLUS:  # 如果操作符类型是加法
            # 分别访问左侧和右侧的记号，将获取的值进行加法运算，并返回结果。
            return self.visit(node.left) + self.visit(node.right)
        elif node.op.type == MINUS:
            return self.visit(node.left) - self.visit(node.right)
        elif node.op.type == MUL:
            return self.visit(node.left) * self.visit(node.right)
        elif node.op.type == DIV:
            return self.visit(node.left) // self.visit(node.right)

    def visit_Num(self, node):  # 访问数字类型节点的方法
        return node.value

    def visit_UnaryOp(self, node):  # 访问一元运算符类型节点的方法
        op = node.op.type
        if op == PLUS:
            return +self.visit(node.expr)
        elif op == MINUS:
            return -self.visit(node.expr)

    def visit_Compound(self, node):  # 访问复合语句节点
        for child in node.children:  # 遍历复合语句节点的子节点
            self.visit(child)  # 访问子节点

    def visit_Assign(self, node):  # 访问赋值语句节点
        var_name = node.left.name  # 获取变量名称
        right = self.visit(node.right)
        var_type = '<class ' + "'" + self.VAR_TYPE[var_name] + "'" + '>'
        if (var_type == str(type(right))):  # 判断变量的类型与变量的赋值是否一致
            self.GLOBAL_SCOPE[var_name] = right  # 以变量名称为键添加变量值到符号表
        else:
            self.error(var_name)  # 不一致报错

    def visit_Variable(self, node):  # 访问变量节点
        var_name = node.name  # 获取变量名称
        value = self.GLOBAL_SCOPE.get(var_name)  # 获取变量值
        if value is None:  # 如果没有返回值（变量不存在）
            raise NameError(f'错误的标识符：{repr(var_name)}')  # 抛出异常
        else:  # 否则
            return value  # 返回变量值

    def visit_Vartype(self, node):  # 访问变量类型节点
        self.VAR_TYPE[node.name] = node.type  # 存储变量的类型和名称

    def visit_NoOp(self, node):  # 访问空语句节点
        pass  # 无操作

    def interpret(self):  # 执行解释的方法
        tree = self.parser.parse()  # 获取语法分析器分析后的树对象
        return self.visit(tree)  # 返回访问语法树最终的遍历结果


def main():
    text = '''
    PROGRAM Part10;
    VAR
        number     : INTEGER;
        a, b, c, x : INTEGER;
        y          : REAL;

    BEGIN {Part10}
        BEGIN
            number := 2;
            a := number;
            b := 10 * a + 10 * number DIV 6;
            c := a - - b
        END;
    x := 11;
    y := 20 / 7 + 3.14;
    { writeln('a = ', a); }
    { writeln('b = ', b); }
    { writeln('c = ', c); }
    { writeln('number = ', number); }
    { writeln('x = ', x); }
    { writeln('y = ', y); }
    END.  {Part10}
    '''
    lexer = Lexer(text)
    parser = Parser(lexer)
    interpreter = Interpreter(parser)
    interpreter.interpret()
    print('VAR_TYPE:', interpreter.VAR_TYPE)  # 显示输出符号表
    print('GLOBAL_SCOPE:', interpreter.GLOBAL_SCOPE)  # 显示输出符号表


if __name__ == '__main__':
    main()
