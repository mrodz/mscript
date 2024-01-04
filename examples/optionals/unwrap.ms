a: int? = nil

bbb: int? = nil

if bbb ?= a {
    print bbb
} else {
    print "a is nil"
}


x = 5
y: int? = nil
print y ?= 10

print x + y

