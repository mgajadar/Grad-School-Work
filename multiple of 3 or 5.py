def multiples(number):
    x =  number
    of5 = []
    of3 = []

    for i in range(3, number + 1, 3):
        of3.append(i)
    for i in range(5, number + 1, 5):
        of5.append(i)

    of3and5 = list(set(of3) | set(of5))
    print (of3and5)
    print (f"Sum of all multiples including",number,":")
    return sum(of3and5)

######### main code ###########
n = int(input("Pick a number "))
if n == 0:
    print(f"Error: ", n )
else:
    print (multiples(n))