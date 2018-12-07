import csv
import statistics
import math


def lcg(a,c,m):
    ''' lcg(int,int,int) -> list of int
    this function is the recursive algorithm (Linear Congruential
    Generator) that generates 200 random values between 0 and 1
    following a uniform distribution. The initial Z value is defined
    within the fucntion'''
    listz=[53]
    for i in range (1,201):
        x = (a*listz[i-1]+c)
        z = x%m
        listz.append(z)
    listu=[]
    for r in listz:
        u= r/m
        listu.append(u)
    listu = listu[1:]
    return listu
    
def Simulate():
    
    '''this function simulates the expected profit for a customer
    given the different catalogue strategies. It runs 10 replications
    of simulation of 5 years, and returns the expected profit over a
    5 year period'''
    custovrtime = [[],[],[],[]]
    profits=[[],[],[],[]]
    averages=[[],[],[],[]]
    Expected=[]
    CIs= [[],[],[],[]]
    #Send only to low category customers 
    state = "l"
    f= lcg(5,9,128)
    for i in range(10):
        
        state = "l"
        custovrtime[0].append([])
        for x in range(20):        
            j=0
            if state is "h" and j is 0:
                if f[i*20 + x] <= 0.4:
                    state = "h"
                    custovrtime[0][i].append(state)
                    j+=1
                else:
                    state = "l"
                    custovrtime[0][i].append(state)
                    j+=1
            if state is "l" and j is 0:
                if f[i*20 + x] <= 0.3:
                    state = "l"
                    custovrtime[0][i].append(state)
                    j+=1
                else:
                    state = "h"
                    custovrtime[0][i].append(state)
                    j+=1
                    
    '''this portion creates a list of profits corresponding with the custovrtime
    list of states'''
    for y in range(0,10):
        profits[0].append([])
        for p in range(0,20):
            if custovrtime[0][y][p] is "h":
                profits[0][y].append(25)
            if custovrtime[0][y][p] is "l":
                profits[0][y].append(5)

    '''this portion creats a list of the average profits from each replication, as
    well as, the overall expected quarterly profit for the given strategy'''
    for k in range(0,10):
        sumprofit = 0
        averages[0].append(0)
        for n in range(0,20):
            sumprofit+= profits[0][k][n]
            averages[0][k] = sumprofit/20 
    expected = sumprofit/20
    Expected.append(expected)

    '''this portion calculates the 95% CI associated with the expected quarterly
    profit'''
    CIlow = (expected)-(2.262*(statistics.stdev(averages[0])/(math.sqrt(10))))
    CIhigh = (expected)+(2.262*(statistics.stdev(averages[0])/(math.sqrt(10))))
    CIs[0].append(round(CIlow,3))
    CIs[0].append(round(CIhigh,3))                    

    '''this portion prints the expected return and 95% CI to the user'''
    print("Expected profit if sending catalogues to only low customers \
is "+str(Expected[0])+" dollars the 95% confidence interval is \
"+"("+str(CIs[0][0])+" , "+(str(CIs[0][1])+")"))

    #Send to only high level customers
    state = "l"
    f= lcg(5,9,128)
    for i in range(10):
        state = "l"
        custovrtime[1].append([])
        for x in range(20):        
            j=0
            if state is "h" and j is 0:
                if f[i*20 + x] <= 0.8:
                    state = "h"
                    custovrtime[1][i].append(state)
                    j+=1
                else:
                    state = "l"
                    custovrtime[1][i].append(state)
                    j+=1
            if state is "l" and j is 0:
                if f[i*20 + x] <= 0.5:
                    state = "l"
                    custovrtime[1][i].append(state)
                    j+=1
                else:
                    state = "h"
                    custovrtime[1][i].append(state)
                    j+=1
                    
    '''this portion creates a list of profits corresponding with the custovrtime
    list of states'''
    for y in range(0,10):
        profits[1].append([])
        for p in range(0,20):
            if custovrtime[1][y][p] is "h":
                profits[1][y].append(25)
            if custovrtime[1][y][p] is "l":
                profits[1][y].append(5)

    '''this portion creats a list of the average profits fro each replication, as
    well as, the overall expected quarterly profit for the given strategy'''
    for k in range(0,10):
        sumprofit = 0
        averages[1].append(0)
        for n in range(0,20):
            sumprofit+= profits[1][k][n]
            averages[1][k] = sumprofit/20 
    expected = sumprofit/20
    Expected.append(expected)

    '''this portion calculates the 95% CI associated with the expected quarterly
    profit'''
    CIlow = (expected)-(2.262*(statistics.stdev(averages[1])/(math.sqrt(20))))
    CIhigh = (expected)+(2.262*(statistics.stdev(averages[1])/(math.sqrt(20))))
    CIs[1].append(round(CIlow,3))
    CIs[1].append(round(CIhigh,3))                    

    '''this portion prints the expected return and 95% CI to the user'''
    print("Expected profit if sending catalogues to only high customers \
is "+str(Expected[1])+" dollars the 95% confidence interval is \
"+"("+str(CIs[1][0])+" , "+(str(CIs[1][1])+")"))

    #Send to neither level of customer
    state = "l"
    f= lcg(5,9,128)
    for i in range(10):
        state = "l"
        custovrtime[2].append([])
        for x in range(20):        
            j=0
            if state is "h" and j is 0:
                if f[i*20 + x] <= 0.4:
                    state = "h"
                    custovrtime[2][i].append(state)
                    j+=1
                else:
                    state = "l"
                    custovrtime[2][i].append(state)
                    j+=1
            if state is "l" and j is 0:
                if f[i*20 + x] <= 0.5:
                    state = "l"
                    custovrtime[2][i].append(state)
                    j+=1
                else:
                    state = "h"
                    custovrtime[2][i].append(state)
                    j+=1
                    
    '''this portion creates a list of profits corresponding with the custovrtime
    list of states'''
    for y in range(0,10):
        profits[2].append([])
        for p in range(0,20):
            if custovrtime[2][y][p] is "h":
                profits[2][y].append(25)
            if custovrtime[2][y][p] is "l":
                profits[2][y].append(5)

    '''this portion creats a list of the average profits from each replication, as
    well as, the overall expected quarterly profit for the given strategy'''
    for k in range(0,10):
        sumprofit = 0
        averages[2].append(0)
        for n in range(0,20):
            sumprofit+= profits[2][k][n]
            averages[2][k] = sumprofit/20 
    expected = sumprofit/20
    Expected.append(expected)

    '''this portion calculates the 95% CI associated with the expected quarterly
    profit'''
    CIlow = (expected)-(2.262*(statistics.stdev(averages[2])/(math.sqrt(10))))
    CIhigh = (expected)+(2.262*(statistics.stdev(averages[2])/(math.sqrt(10))))
    CIs[2].append(round(CIlow,3))
    CIs[2].append(round(CIhigh,3))                    

    '''this portion prints the expected return and 95% CI to the user'''
    print("Expected profit if sending catalogues to neither typr of customer \
is "+str(Expected[2])+" dollars the 95% confidence interval is \
"+"("+str(CIs[2][0])+" , "+(str(CIs[2][1])+")"))

    #Send to both level of customer
    state = "l"
    f= lcg(5,9,128)
    for i in range(10):
        state = "l"
        custovrtime[3].append([])
        for x in range(20):        
            j=0
            if state is "h" and j is 0:
                if f[i*20 + x] <= 0.8:
                    state = "h"
                    custovrtime[3][i].append(state)
                    j+=1
                else:
                    state = "l"
                    custovrtime[3][i].append(state)
                    j+=1
            if state is "l" and j is 0:
                if f[i*20 + x] <= 0.3:
                    state = "l"
                    custovrtime[3][i].append(state)
                    j+=1
                else:
                    state = "h"
                    custovrtime[3][i].append(state)
                    j+=1
                    
    '''this portion creates a list of profits corresponding with the custovrtime
    list of states'''
    for y in range(0,10):
        profits[3].append([])
        for p in range(0,20):
            if custovrtime[3][y][p] is "h":
                profits[3][y].append(25)
            if custovrtime[3][y][p] is "l":
                profits[3][y].append(5)

    '''this portion creats a list of the average profits fro each replication, as
    well as, the overall expected quarterly profit for the given strategy'''
    for k in range(0,10):
        sumprofit = 0
        averages[3].append(0)
        for n in range(0,20):
            sumprofit+= profits[3][k][n]
            averages[3][k] = sumprofit/20 
    expected = sumprofit/20
    Expected.append(expected)

    '''this portion calculates the 95% CI associated with the expected quarterly
    profit'''
    CIlow = (expected)-(2.262*(statistics.stdev(averages[3])/(math.sqrt(10))))
    CIhigh = (expected)+(2.262*(statistics.stdev(averages[3])/(math.sqrt(10))))
    CIs[3].append(round(CIlow,3))
    CIs[3].append(round(CIhigh,3))                    

    '''this portion prints the expected return and 95% CI to the user'''
    print("Expected profit if sending catalogues both type of customer \
is "+str(Expected[3])+" dollars the 95% confidence interval is \
"+"("+str(CIs[3][0])+" , "+(str(CIs[3][1])+")"))

    
Simulate()
    
