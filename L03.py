# 编写简单解释器进阶程序代码

# 2021-06-10
# 支持一位数加法:7+8
# 增加eat方法，让代码结构更好(向鲁斯兰的方法靠拢)
# 支持一位数减法:8-3、
# 支持两个操作数字之间空格如：9   +    3
# 支持任意位数字长度如: 8888  -  222
# Myversion  2021-06-10

# 把我自己写的支持减法、空格和任意位数字长度等功能的程序，修改为按鲁斯兰的方法实现
# Stdversion  2021-06-15

# 支持连续的加减法:如12 + 8 – 6 – 10
# Myversion  2021-06-16

# 把我自己写的支持连续的加减法:如12 + 8 – 6 – 10,修改为按鲁斯兰的方法实现
# Stdversion  2021-06-17


INTEGER, PLUS, MINUS, EOF = 'INTEGER', 'PLUS', 'MINUS', 'EOF'  # 整数，加法，减法，结束标识


class Token:  # 定义记号类
    def __init__(self, value_type, value):  # 定义构造方法
        self.value_type = value_type  # 记号中值的类型
        self.value = value  # 记号中的值

    def __str__(self):  # 重写查看记号内容的方法
        return 'Token({value_type},{value})'.format(value_type=self.value_type, value=self.value)

    def __repr__(self):  # 也可以写成 __repr__=__str__
        return self.__str__()


class Interpreter:
    def __init__(self, text):  # 定义构造方法获取用户输入的表达式
        self.text = text  # 用户输入的表达式
        self.position = 0  # 获取表达式中每一个字符时的位置
        self.current_token = None  # 临时保存记号的变量
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

    def long_integer(self):  # 获取多位数字
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
                return Token(INTEGER, self.long_integer())  # 获取完整的数字创建记号对象并返回
            if self.current_char == '+':  # 如果当前字符是加号
                self.advance()  # 跳到下一字符
                return Token(PLUS, self.current_char)  # 创建记号对象并返回
            if self.current_char == '-':  # 如果当前字符是减号
                self.advance()  # 跳到下一字符
                return Token(MINUS, self.current_char)  # 创建记号对象并返回
            self.error()  # 如果以上都不是，则抛出异常。
        return Token(EOF, None)  # 遍历结束返回结束标识创建的记号对象

    def eat(self, token_type):
        if (self.current_token.value_type == token_type):
            self.current_token = self.get_next_token()
        else:  # 否则
            self.error()  # 抛出异常

    def term(self):
        token = self.current_token  # 获取记号
        self.eat(INTEGER)
        return token.value  # 返回记号中的整数值

    def expr(self):
        self.current_token = self.get_next_token()

        result = self.term()  # 获取第一个整数（Term）

        while self.current_token.value_type in (PLUS, MINUS):
            if self.current_token.value_type == PLUS:
                self.eat(PLUS)
                result += self.term()  # 已有结果加上新获取的整数（Term）
            if self.current_token.value_type == MINUS:
                self.eat(MINUS)  # 已有结果减去新获取的整数（Term）
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

        interpreter = Interpreter(text)
        result = interpreter.expr()
        print(text, '=', result)


if __name__ == '__main__':
    main()
