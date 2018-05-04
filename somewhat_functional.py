from __future__ import print_function

# operator->function
def cat(s1,s2):
	return s1 + s2

# this is one way to curry a function
def carg1(func, arg1):
	return lambda x: func(arg1, x)

def carg2(func, arg2):
	return lambda x: func(x, arg2)

# immediately applying the returned function, as in the following, is pretty pointless
for word in sentence:
	# print(zip(range(10), list(word))
	print(carg1(zip, range(10))(list(word)))
	
def space_cat(s1, s2):
	# I just like to write "space_cat"
	# we could do carg2(cat, s2)(carg1(cat, s1)(" "))
	# or s1 + " " + s2
	# but, this works, too
	return cat(s1, cat(" ", s2))


def cspace_cat1(s1):
	return lambda x: space_cat(s1, x)

def cspace_cat2(s2):
	return lambda x: space_cat(x, s2)

print( cspace_cat1("This may be")("overkill.") )
print( cspace_cat2('Yoda ordering.')("It can have") )

# which is all fine and good, but we can start doing more
say_hi = cspace_cat1("Hey there,")
people =  ["Matt", "Lenny", "Candice"]
for person in people:
	print(say_hi(person))

def car(li):
	return li[0]

def cdr(li):
	return li[1:]

def com_cat(s1, s2):
	# not to be confused with concat, com-sat, or Comcast
	return carg2(cat, s2)(carg1(cat, s1)(", ")) if s2 else s1

def comma_listify_with_grammatical_and(li):
	# copy to avoid stomping on callers list
	cop_li = li[:]
	cop_li[-1] = space_cat('and', cop_li[-1])
	def inner(li):
		return com_cat(car(li), inner(cdr(li))) if li else "" 
	return inner(cop_li)

print(say_hi(comma_listify_with_grammatical_and(people)))

# next time we'll get to cons...
def cons(x, li):
	return [x] + li

