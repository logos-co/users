import math
from scipy.stats import binom
def comm(N,delta,A,P):
#N is the number of nodes, delta is the failure prob. which can be tolerated, A is the fraction of a committee (typical value is  1/3) and P  is the fraction of adversarial nodes (typical value is 1/4). 
    K = 1
    n = N
    r = 0
    Prob = 0.0
    while Prob < delta:
        K_1 = K
        n_1 = n
        r_1 = r
        Prob_1 = Prob
        K = K + 1
        n = N // K
        r = N % K
        if 0 < r:
            Prob_n = binom.cdf(math.floor(A * n), n, P)
            Prob_n1 = binom.cdf(math.floor(A * (n+1)), n+1, P)
            Prob = 1 - Prob_n ** (K - r) * Prob_n1 ** r
        else:
            Prob_n = binom.cdf(math.floor(A * n), n, P)
            Prob = 1 - Prob_n ** K
    #return number of committees, K_1, committee size, n_1, number of committees with size n_1+1, r_1 and prob. of failure, Prob_1. 
    return K_1, n_1, r_1, Prob_1;
