import math 

f = int (input("What is the principle deposit amount?"))
#print(f) #for debug to check value
p = int (input("What is the interest percentage amount?"))
#print(p) #for debug to check value
c = int (input("What is base withdrawl amount?"))
#print(c) #for debug to check value
n = int (input("How many years are you planning for?"))
#print(n) #for debug to check value
i = int (input("What is the inflation rate?"))
#print(i) #for debug to check value

def fortune(f,p,c,n,i):
    year = 1 # beginning of year 2
    p = p/100
    i = i/100
    #print(p, i) #debug check value
    while year < n:
        f = (f * (1 + p)) - c
        c =  c * (1 + i) 
        if f < 0 or f < c:
            print("Year", year, "- Bad plan! Balance =", math.trunc(f), "Expenses =", math.trunc(c))
            break  # Stop the loop if balance is too low
        print("Year ", year + 1, "balance = ", math.trunc(f))
        print("Year ", year + 1, "expenses = ", math.trunc(c))
        year += 1

 
fortune(f,p,c,n,i)
