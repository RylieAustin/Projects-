import csv
import statistics

def lcg(a,c,m):
    ''' lcg(int,int,int) -> list of int
    this function is the recursive algorithm (Linear Congruential
    Generator) that generates 30 random values between 0 and 1
    following a uniform distribution. The initial Z value is defined
    within the fucntion'''
    listz=[71]
    for i in range (1,31):
        x = (a*listz[i-1]+c)
        z = x%m
        listz.append(z)
    listu=[]
    for r in listz:
        u= r/m
        listu.append(u)
    listu = listu[1:]
    return listu

def light_clr(a,c,m):
    '''light_clr(int,int,int) -> str
    this function calls the lcg function above and uses the
    corresponding list on RGN's to associate outcome variables,
    which in this case are categorical variables corresponding
    to traffic light colours'''
    listu = lcg(a,c,m)
    lights=[]
    for p in listu:
        if p <= .10:
            lights.append('yellow')
        if .10 < p and p <= .50:
            lights.append('green')
        elif .50 < p and p <= 1.0:
            lights.append('red')

    Y = lights.count('yellow')
    G = lights.count('green')
    R = lights.count('red')

    print(lights)

    return 'There are:'+str(Y)+' yellow lights, '+str(G)+' green lights, and '+str(R)+' red lights!'


def stats():
    
    f= lcg(5,9,128)
    m = statistics.mean(f)
    v = statistics.variance(f)
    print('mean='+str(m))
    print('Variance='+str(v))
    







        
        
            
