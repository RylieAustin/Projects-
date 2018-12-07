import csv
import statistics as stats
import math
import numpy as np

##### Assignment  4
'''create matrix that show the rate at wich the system transitions from
on state to another, sims is the number of repications, sim is the
duration of each replication'''

ratrix =[[0,30,0,0],
        [40,0,30,0],
        [0,80,0,30],
        [0,0,80,30]]
np.random.seed(281)
    
def Simulate(ratrix):
    '''this function simulates the state flow of the system and the
    time spent in each state,. It runs 100 replications
    of simulations lasting 8 hours, and returns the expected values over a
    all replications'''
    statepath = []
    timeinstate = []
    repstateprobs=[]
    stateprobs=[[],[],[],[]]
    stateAVs = []
    stateCIs= [[],[],[],[]]
    repscallsreceived = []
    repscallsaccepted = []
    repslost = []
    repswaiting = []
    Pi3s = []
    repswaitingAV = []
    
    for i in range(1000): 
        state=0
        simtime=0
        statepath.append([state])
        timeinstate.append([])
        while simtime < 8:
            rand = np.random.random()
            sojourn = np.random.exponential(1/(np.sum(ratrix[state])))
            simtime += sojourn
            for j in range(len(ratrix[state])):
                if rand <= np.sum(ratrix[state][:j+1]/np.sum(ratrix[state])):
                    state = j
                    timeinstate[i].append(sojourn)
                    statepath[i].append(state)
                    break
        statepath[i] = statepath[i][:-1]
        timeinstate[i] = timeinstate[i][:-1]
        timeinstate[i].append(8-np.sum(timeinstate[i]))


    '''this portioon calculates the probability of being in a given state
    for each state path in each replication. It then takes those probablitities
    and calculates the overall probability for states amongst all replications
    and provides the 95% confidence interval.'''
    for i in range(len(statepath)):
        repstateprobs.append([[],[],[],[]])
        nocalls = 0
        onecall = 0
        twocall = 0
        onhold  = 0
        for j in range(len(statepath[i])):
            if statepath[i][j] == 0:
                nocalls += timeinstate[i][j]
                repstateprobs[i][0] = (nocalls / sum(timeinstate[i]))
            if statepath[i][j] == 1:
                onecall += timeinstate[i][j]
                repstateprobs[i][1] = (onecall / sum(timeinstate[i]))
            if statepath[i][j] == 2:
                twocall += timeinstate[i][j]
                repstateprobs[i][2] = (twocall / sum(timeinstate[i]))
            if statepath[i][j] == 3:
                onhold += timeinstate[i][j]
                repstateprobs[i][3] = (onhold / sum(timeinstate[i]))
       
    for i in range(len(repstateprobs)):
        stateprobs[0].append(repstateprobs[i][0])
        stateprobs[1].append(repstateprobs[i][1])
        stateprobs[2].append(repstateprobs[i][2])
        stateprobs[3].append(repstateprobs[i][3])

    for y in range(4):
        stateAVs.append(np.average(stateprobs[y]))
        
    for k in range(4):
        stateCIs[k].append(stateAVs[k] - ((1.96)*((np.std(stateprobs[k],ddof=1)/np.sqrt(1000)))))
        stateCIs[k].append(stateAVs[k] + ((1.96)*((np.std(stateprobs[k],ddof=1)/np.sqrt(1000)))))


    #print(stateAVs)
    #print(stateCIs)


    '''this portion counts the % total of calls that the system balks by taking the
    total time in state 3 and dividing by the total tinme of the replication. After this, it
    Uses this metric for all replications to returnt the average percentage number of calls
    that are not accepted by the system and the 95% CI.'''

    
    for p in range(len(statepath)):
        repscallsreceived.append(0)
        repscallsaccepted.append(0)
        for q in range(1,len(statepath[p])):
            n = statepath[p][q]
            m = statepath[p][q-1]
            if m <= n:
                   repscallsreceived[p] += 1
            if m < n:
                   repscallsaccepted[p] += 1
                   
    for z in range(len(repscallsreceived)):
        repslost.append((repscallsreceived[z] - repscallsaccepted[z])/repscallsreceived[z])

    callslostAV = np.average(repslost)
    callslostCI = [ (callslostAV - ((1.96)*((np.std(repslost,ddof=1)/np.sqrt(1000))))) ,
                    (callslostAV + ((1.96)*((np.std(repslost,ddof=1)/np.sqrt(1000)))))]

    print('Average % calls balked is:'+ str(round(callslostAV,5)))
    print('The corresponfing CI is: ('+ str(round(callslostCI[0],5))+' , '+str(round(callslostCI[1],5))+ ')')
        
            
    '''this portion calculates the total wait time for each replication by adding the sojourn times
    for state 3. It then uses those values to calculate the overall average waiting time and the 95%
    CI.'''

    for r in range(len(statepath)):
        repswaiting.append([])
        for t in range(len(statepath[r])):
            if statepath[r][t] == 3:
                repswaiting[r].append(timeinstate[r][t])

    for b in range(len(repswaiting)):
        repswaitingAV.append((np.sum(repswaiting[b]))/(repscallsaccepted[b])*60)

    callswaitingAV = np.average(repswaitingAV)
    callswaitingCI = [ (callswaitingAV - ((1.96)*((np.std(repswaitingAV,ddof=1)/np.sqrt(1000))))) ,
                    (callswaitingAV + ((1.96)*((np.std(repswaitingAV,ddof=1)/np.sqrt(1000)))))]

    #print('Average waiting time is:'+ str(round(callswaitingAV,5))+' minutes')
    #print('The corresponfing CI is: ('+ str(round(callswaitingCI[0],5))+' , '+str(round(callswaitingCI[1],5))+ ')')
    return
                                      
Simulate(ratrix)
