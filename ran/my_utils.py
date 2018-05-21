
# I just wanted to see how I might implement argument checking.

def func_arg_type_error_message(argname, expected_type, received_object):
	return "argument %s is not type %s, it is %s with repr of %s" % (argname, expected_type, str(type(received_object)), str(received_object))

def cat(s1,s2):
	for x in dir():
		assert eval('type(' + x + ')') == type(""), (func_arg_type_error_message(x, type(""), eval(x)))
	return s1 + s2
