import random
for n in range(1050):
    i = [random.uniform(0.0, 100.0) for x in range(5)]
    i = " ".join(format(x, "15.6f") for x in i)
    o = random.uniform(0.0, 100.0)
    print "%12d %s %15.6f" % (n, i, o)
