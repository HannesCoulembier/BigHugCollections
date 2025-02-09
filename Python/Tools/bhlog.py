from functools import wraps
from enum import IntEnum

class Severity(IntEnum):
	"""Specifies how the logger should handle a msg"""
	info=0
	warn=1
	error=2
	crash=3


def log(msg:str, severity:Severity)-> None:
	"""Logs msg according to severity
	
	args:
		msg (str): The message to be displayed
		severity (Severity): Specifies how to format the msg
	return:
		None

	TODO: format according to severity
	"""

	if severity == Severity.info:
		print(msg)
	elif severity == Severity.warn:
		print("Warning:", msg)
	elif severity == Severity.error:
		print("Error:", msg)
	elif severity == Severity.crash:
		raise Exception(msg)
	else:
		raise Exception("Log was called using an unknown severity value")


def msgOnNone(msg:str, severity:Severity)->callable:
	"""Will log 'msg' with given 'severity' when the result of the decorated function is None
	
	args:
		msg (str): The message to display when function returns None
		severity (Severity): The severity of the situation when function returns None
	return:
		(callable): A decorator that will apply the behaviour described above
	"""
	def decorator(f:callable)->callable:
		@wraps(f)
		def wrapper(*args, **kwargs):
			res = f(*args, *kwargs)
			if res == None:
				log(msg, severity)
			return res
		return wrapper
	return decorator