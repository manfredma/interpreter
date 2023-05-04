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


INTEGER, PLUS, MINUS, MUL, DIV, LPAREN, RPAREN, EOF = 'INTEGER', 'PLUS', 'MINUS', 'MUL', 'DIV', 'LPAREN', 'RPAREN', 'EOF'  # 整数，加法，减法，乘法、除法，左括号,右括号,结束标识


# ------词法分析器Lexer部分------
class Token:  # 定义记号类
    def __init__(self, value_type, value):  # 定义构造方法
        self.value_type = value_type  # 记号中值的类型
        self.value = value  # 记号中的值

    def __str__(self):  # 重写查看记号内容的方法
        return 'Token({value_type},{value})'.format(value_type=self.value_type, value=self.value)

    def __repr__(self):  # 也可以写成 __repr__=__str__
        return self.__str__()


class Lexer:  # 词法分析器
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

    def integer(self):  # 获取多位数字
        result = ''
        while self.current_char is not None and self.current_char.isdigit():  # 如果当前字符不是None值并且当前字符是数字
            result += self.current_char  # 连接数字
            self.advance()  # 获取下一个字符
        return int(result)  # 返回数字

    def get_next_token(self):
        while self.current_char is not None:  # 如果当前字符不是None值
            if self.current_char.isspace():  # 如果当前字符是空格
                self.skip_whitespace()  # 跳过所有空格
                continue
            if self.current_char.isdigit():  # 如果当前字符是整数
                return Token(INTEGER, self.integer())  # 获取完整的数字创建记号对象并返回
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
            self.error()  # 如果以上都不是，则抛出异常。
        return Token(EOF, None)  # 遍历结束返回结束标识创建的记号对象


# ------语法分析器Parser部分------
class Tree:  # 定义抽象语法树
    def __init__(self, left, node, right):  # 定义构造方法
        self.left = left  # 树左节点
        self.node = node  # 树根节点
        self.right = right  # 树右节点

    def visit(self):  # 二叉树遍历
        while not (self.left is None and self.right is None):  # 不是叶节点就一直递归调用
            print(self.left, self.node, self.right)
            try:
                self.left = self.left.visit()  # 树左节点一直递归
                self.right = self.right.visit()  # 树右节点一直递归
            except:  # 递归到叶节点
                break
        # 已经递归到了叶节点
        if self.node == PLUS:  # 加法树
            result = self.left + self.right
        if self.node == MINUS:  # 减法树
            result = self.left - self.right
        if self.node == MUL:  # 乘法树
            result = self.left * self.right
        if self.node == DIV:  # 除法树
            result = self.left / self.right
        return result


class Parser:  # 语法分析器
    def __init__(self, lexer):  # 定义构造方法获取用户输入的表达式
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()  # 语法分析器初始化

    def eat(self, token_type):
        if self.current_token.value_type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:  # 否则
            self.lexer.error()  # 抛出异常

    def factor(self):  # 语法分析器最底层结构：整数或括号
        token = self.current_token  # 获取记号
        if token.value_type == INTEGER:  # 整数
            self.eat(INTEGER)
            tree = Tree(token.value, PLUS, 0)  # 生成整数叶节点树
            return tree  # 返回叶节点树
        elif token.value_type == LPAREN:  # 左括号
            self.eat(LPAREN)
            tree = self.expr()  # 求出括号里面的AST树
            self.eat(RPAREN)  # 右括号
            return tree  # 返回括号内的AST树

    def term(self):  # 语法分析器中间层结构：乘除
        tree = self.factor()  # 获取第一个数字树,如没有乘除法，将直接返回一个代表数字的叶节点树
        while self.current_token.value_type in (MUL, DIV):
            token = self.current_token
            if token.value_type == MUL:
                self.eat(MUL)
                # 生成新的树：把目前已经获取到的乘除法树整体做为左子树，起到连续乘的作用
                tree = Tree(tree, MUL, self.factor())
                # 新的树以取得新的数字或括号内的树为右子树
            if token.value_type == DIV:
                self.eat(DIV)
                # 生成新的树：把目前已经获取到的乘除法树整体做为左子树，起到连续除的作用
                tree = Tree(tree, DIV, self.factor())
                # 新的树以取得新的数字或括号内的树为右子树
        return tree

    def expr(self):  # 语法分析器最高层结构：加减
        tree = self.term()  # 获取第一段乘除
        while self.current_token.value_type in (PLUS, MINUS):
            token = self.current_token
            if token.value_type == PLUS:
                self.eat(PLUS)
                # 生成新的树：把目前已经获取到的加减法树整体做为左子树，起到连续加的作用
                tree = Tree(tree, PLUS, self.term())  # 获取乘除
                # 新的树以取得新的乘除为右子树
            if token.value_type == MINUS:
                self.eat(MINUS)
                # 生成新的树：把目前已经获取到的加减法树整体做为左子树，起到连续减的作用
                tree = Tree(tree, MINUS, self.term())  # 获取乘除
                # 新的树以取得新的乘除为右子树
        return tree


# ------解释器Interpreter部分------
class Interpreter:
    def __init__(self, parser):  # 定义构造方法获取用户输入的表达式
        self.parser = parser

    def interpret(self):
        # 通过语法分析器最顶层方法expr()生成抽象语法树AST，输入到解释器
        tree = self.parser.expr()
        return tree.visit()  # 遍历最终的语法树得出计算结果


def main():
    while True:  # 循环获取输入
        try:
            text = input('>>>')  # 获取用户输入
        except EOFError:  # 捕获到末端错误时退出
            break
        if not text:  # 如果未输入时继续提示输入
            continue

        lexer = Lexer(text)  # 词法分析器从用户输入获得表达式
        parser = Parser(lexer)  # 语法分析器从词法分析器获得Token
        interpreter = Interpreter(parser)  # 解释器从语法分析器获得AST
        result = interpreter.interpret()  # 解释器遍历AST计算出结果
        print(text, '=', result)


if __name__ == '__main__':
    main()
