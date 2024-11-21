import Tools.color as color
import Tools.io as io

import os

print(os.getcwd())

file = io.File("Python\sadbox.py")
print(file.exists())
print(file.readStr())