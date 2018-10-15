#language learning app developed with BeeWare for learning mandarin chinese. Tests english to pinyin and chinese characters, which is largely lacking from duolingo.

import toga
from toga.style import Pack
from toga.style.pack import COLUMN, ROW, CENTER
from time import time

# Handler for answer selection by button pressed
def button_handler(widget):
    print('correct!')


# Main class for running the app
class Triolingo(toga.App):

    def randint(self, limit_lower, limit_upper):
        min = float(0)
        max = float(999)
        x = float(str(time()).split('.')[1][-4:-1])
        x = (x - min) / (max - min)
        x = round((limit_upper - limit_lower) * x)
        return x

    # Loads vocabulary corpus from file and returns dict of lists
    def get_vocab(self, file_path):
        i = 0
        vocab = {}
        file = open(file_path)
        for line in file:
            x = line.split('\t')
            vocab[i] = x
            i += 1
        file.close()
        return vocab

    # Pick question based on random number for now.
    # TODO: pick based on probability based on past performance
    def pick_question(self, vocab_progress):
        index_option1 = self.randint(1, vocab_progress)
        index_option2 = self.randint(1, vocab_progress)
        index_option3 = self.randint(1, vocab_progress)
        index_option4 = self.randint(1, vocab_progress)

        option1 = self.vocab[index_option1]
        option2 = self.vocab[index_option2]
        option3 = self.vocab[index_option3]
        option4 = self.vocab[index_option4]

        question_content = {}
        question_content['1'] = (option1[5], option1[2])
        question_content['2'] = (option2[5], option2[2])
        question_content['3'] = (option3[5], option3[2])
        question_content['4'] = (option4[5], option4[2])

        return question_content

    # Runs on startup to construct the GUI
    def startup(self):

        # Create a main window with a name matching the app
        self.main_window = toga.MainWindow(title=self.name)
        self.file_path = '/Users/laurens.geffert/local/personal/triolingo/data/vocab_processed.txt'
        self.vocab = self.get_vocab(file_path=self.file_path)
        self.question_content = self.pick_question(vocab_progress=300)

        # Create a main content box
        box_main = toga.Box(style=Pack(direction=COLUMN, alignment=CENTER))
        # Generate question line
        question_text = self.question_content['1'][0]
        question = toga.Label(text=question_text, style=Pack(text_align=CENTER))
        box_main.add(question)
        # Generate answer options
        box_row1 = toga.Box(style=Pack(direction=ROW, alignment=CENTER))
        box_row2 = toga.Box(style=Pack(direction=ROW, alignment=CENTER))
        # Generate answer buttons
        n = 4
        for i in range(n):
            button = toga.Button(
                label=self.question_content[f'{i+1}'][1],
                id=i,
                on_press=None,
                style=Pack(font_size=50))
            button.style.padding = 10

            if i < 2:
                box_row1.add(button)
            else:
                box_row2.add(button)

        # Add row boxes to main box
        box_main.add(box_row1)
        box_main.add(box_row2)
        # Add the content on the main window
        self.main_window.content = box_main
        # Show the main window
        self.main_window.show()


def main():
    return Triolingo('Triolingo', 'net.triolingo.triolingo')



