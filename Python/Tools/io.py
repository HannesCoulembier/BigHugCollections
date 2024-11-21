import Tools.log as log

import os
from typing import Union

class File:
    def __init__(self, path:os.path):
        self.path = path

    def exists(self)->bool:
        """Returns whether or not the file actually exists"""
        return os.path.isfile(self.path)
    
    @log.msgOnNone("File does not exist", log.Severity.error)
    def readStr(self)->Union[str, None]:
        """Reads content of file located at path
        
        return:
            content of file if it exists, else None
        """
        if self.exists():
            return readFileStr(str(self.path))
        return None
    
    def __str__(self) -> str:
        return str(self.path)

def readFileStr(location:str)->str:
    """Reads the contents of the file at 'location' as a string
    
    args:
        location (str): The location of the file to read from
    return:
        (str): The contents of the file
    
    If the file doesn't exist, this function will crash
    """
    with open(location, 'r') as f:
        text = f.read()
    return text