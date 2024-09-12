#This code simulates random assignment  of N nodes  into the K committees.
#The number of adversarial nodes is  N_r (N_r < N).
#The code computes the probability that in at least one of the committees the number of adversarial nodes exceeds the A x the number of nodes  in the committee, where  A is the number between 0 and 1.
#For details see https://www.overleaf.com/read/yqvczvrmsgts
#To run code  launch R from the terminal and type  source('rand_part_4_v2.r'); The output is in rand_num* files.
code_name<-'rand_part_4_v2.r';
rand_num=7800; 
set.seed(rand_num);
print(rand_num);
##################parameters##############################################
M=10^6; #number of samples
N=773; #total number of nodes
K=3; #number of committees
N_r=floor(N/4); #number of adversarial nodes
A=1/3; #fraction of nodes  in a committee
n=N%/%K; #compute quotient
r=N%%K; #compute remainder
###################generate array of committee sizes such that K-r committees are of size n and r committees are of size n+1########################
comm=replicate(K,n);
for (k in 1:r) comm[k]=comm[k]+1;
###########################################################################
in_file<-paste(rand_num,"_param.txt",sep="");
out_file<-paste(rand_num,"_stats.csv", sep="");
###########################################################################
write(paste("r-code=", code_name,", rand_num=",rand_num, ", M=",M,", N=",N, ", K=",K,", n=",n, ", r=",r, ", N/N_r=",(N/N_r),", A=", A, sep=""), in_file, ncolumns =1, append = FALSE,  sep = "\n");
write(paste("K", "Prob",  sep = "\t"), out_file, ncolumns =2, append = FALSE,  sep = "\t");
start_time <- Sys.time();
#generate binary sequence with exactly N_r of 1's
s0<-replicate(N,0);
for (i in 1:N_r)
{
    s0[i]=1;
};
Prob=0;
for (j in 1:M)
{
    s<-sample(s0);
    N0<-replicate(K,0);
    N1<-replicate(K,0);
    mu=1;
    cntr=0;
    for (i in 1:N)
    {
        if (s[i]>0)
        {
            N1[mu]=N1[mu]+1;
        } else
                {
                    N0[mu]=N0[mu]+1;
                }
        cntr=cntr+1;
        if (cntr==comm[mu])
        {
            cntr=0;
            mu=mu+1;
        }
                        
    }
                    SUM=0;
                    for (mu in 1:K)
                    {
                        if (N1[mu]>=(floor((A*(N1[mu]+N0[mu])))+1))
                        {
                            SUM=SUM+1;
                        }
                    }
                    if (SUM>0)
                    {
                        Prob=Prob+1;
                    }
}
Prob=Prob/M;
write(paste(K,Prob,sep = "\t"), out_file, ncolumns =1, append =TRUE,  sep = "\t");
end_time <- Sys.time();
print(end_time - start_time);
