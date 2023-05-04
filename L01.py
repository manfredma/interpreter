# This is a sample Python script.

# Press ⌃R to execute it or replace it with your code.
# Press Double ⇧ to search everywhere for classes, files, tool windows, actions, and settings.

INTEGER, PLUS, EOF = 'INTEGER', 'PLUS', 'EOF'  # 整数，加法，结束标识


class Token:  # 定义记号类
    def __init__(self, value_type, value):  # 定义构造方法
        self.value_type = value_type  # 记号中值的类型
        self.value = value  # 记号中的值

    def __str__(self):  # 重写查看记号内容的方法
        return 'Token({value_type},{value})'.format(value_type=self.value_type, value=self.value)

    def __repr__(self):  # 也可以写成 __repr__=__str__
        return self.__str__()


class Interpreter:  # 定义解释器类
    def __init__(self, text):  # 定义构造方法获取用户输入的表达式
        self.text = text  # 用户输入的表达式
        self.position = 0  # 获取表达式中每一个字符时的位置
        self.current_token = None  # 临时保存记号的变量

    def error(self):  # 定义提示错误的方法
        raise Exception('警告：错误的输入内容！')  # 抛出异常

    def get_next_token(self):  # 定义获取记号的方法
        text = self.text
        if self.position >= len(text):  # 如果获取字符的位置已经到达末端
            return Token(EOF, None)  # 返回结束标识的记号对象
        current_char = text[self.position]  # 获取当前位置的字符
        if current_char.isdigit():  # 如果当前位置的字符是数字
            token = Token(INTEGER, int(current_char))  # 实例化整数的记号对象
            self.position += 1  # 获取字符的位置自增1，以便获取下一个字符。
            return token  # 返回记号对象
        if current_char == '+':  # 如果当前位置的字符是加号
            token = Token(PLUS, current_char)  # 实例化加号运算符的记号对象
            self.position += 1  # 获取字符的位置自增1，以便获取下一个字符。
            return token  # 返回记号对象
        self.error()  # 如果以上没有任何对象返回，抛出异常。

    def expr(self):  # 定义验证运算结构并计算结果的方法
        self.current_token = self.get_next_token()  # 获取第一个记号
        left = self.current_token  # 保存第1个记号到变量
        if self.current_token.value_type == INTEGER:  # 如果记号中的值类型是整数
            self.current_token = self.get_next_token()  # 获取下一个记号对象存入变量
        else:  # 否则
            self.error()  # 抛出异常

        operator = self.current_token  # 保存第2个记号到变量
        if self.current_token.value_type == PLUS:  # 如果记号中的值类型是加号
            self.current_token = self.get_next_token()  # 获取下一个记号对象存入变量
        else:  # 否则
            self.error()  # 抛出异常

        right = self.current_token  # 保存第3个记号到变量
        if self.current_token.value_type == INTEGER:  # 如果记号中的值类型是整数
            self.current_token = self.get_next_token()  # 获取下一个记号对象存入变量
        else:  # 否则
            self.error()  # 抛出异常

        result = left.value + right.value  # 进行加法运算获取结果
        return result  # 返回计算结果


def main():
    while True:  # 循环获取输入
        try:
            text = input('>>>')  # 获取用户输入
        except EOFError:  # 捕获到末端错误时退出
            print('bye bye')
            break
        if not text:  # 如果未输入时继续提示输入
            continue
        interpreter = Interpreter(text)  # 实例化解释器对象
        result = interpreter.expr()  # 执行运算方法获取运算结果
        print(text, '=', result)  #


# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    main()

