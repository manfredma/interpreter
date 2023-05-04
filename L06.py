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
# Stdversion  2021-06-18


INTEGER, PLUS, MINUS, MUL, DIV, LPAREN, RPAREN, EOF = 'INTEGER', 'PLUS', 'MINUS', 'MUL', 'DIV', 'LPAREN', 'RPAREN', 'EOF'


# 整数，加法，减法，乘法、除法，左括号,右括号,结束标识


class Token:  # 定义记号类
    def __init__(self, value_type, value):  # 定义构造方法
        self.value_type = value_type  # 记号中值的类型
        self.value = value  # 记号中的值

    def __str__(self):  # 重写查看记号内容的方法
        return 'Token({value_type},{value})'.format(value_type=self.value_type, value=self.value)

    def __repr__(self):  # 也可以写成 __repr__=__str__
        return self.__str__()


class Lexer:
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


class Interpreter:
    def __init__(self, lexer):  # 定义构造方法获取用户输入的表达式
        self.lexer = lexer
        # set current token to the first token taken from the input
        self.current_token = self.lexer.get_next_token()

    def eat(self, token_type):
        if self.current_token.value_type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:  # 否则
            self.lexer.error()  # 抛出异常

    def factor(self):
        token = self.current_token  # 获取记号
        if token.value_type == INTEGER:
            self.eat(INTEGER)
            return token.value  # 返回记号中的整数值

        elif token.value_type == LPAREN:  # 左括号
            self.eat(LPAREN)
            result = self.expr()  # 求出括号里面的表达式值
            self.eat(RPAREN)  # 右括号
            return result

    def term(self):
        result = self.factor()  # 获取第1个整数（factor）
        while self.current_token.value_type in (MUL, DIV):
            token = self.current_token
            if token.value_type == MUL:
                self.eat(MUL)  # 已有结果乘上新获取的整数（factor）
                result *= self.factor()
            if token.value_type == DIV:
                self.eat(DIV)  # 已有结果除以新获取的整数（factor）
                result /= self.factor()
        return result

    def expr(self):
        result = self.term()  # 获取第一段乘除或数字
        while self.current_token.value_type in (PLUS, MINUS):
            token = self.current_token
            if token.value_type == PLUS:
                self.eat(PLUS)
                result += self.term()  # 已有结果加上新获取的乘除（term）
            if token.value_type == MINUS:
                self.eat(MINUS)  # 已有结果减去新获取的乘除（term）
                result -= self.term()
        return result


def main():
    while True:  # 循环获取输入
        try:
            text = input('>>>')  # 获取用户输入
        except EOFError:  # 捕获到末端错误时退出
            break
        if not text:  # 如果未输入时继续提示输入
            continue

        lexer = Lexer(text)
        interpreter = Interpreter(lexer)
        result = interpreter.expr()
        print(text, '=', result)


if __name__ == '__main__':
    main()