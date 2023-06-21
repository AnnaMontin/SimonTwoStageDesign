# SimonTwoStageDesign
RShiny application for modified Simon's two-stage design

## Simon's Two-Stage Design
Let's describe the responses to a treatment with a binary random variable $X∼Bi(n,p)$, where $X_i=1$ with propability $p$ and $X_i=0$ with probability $1−p$ for $i,…,n$. 

A two-stage design is defined by:

- the number of patients to be accrued during stages one and two, $n_1$ and $n_2$
- the the number of responses we observe, the boundary values, $r_1$ and $r$ $(r_1 < r )$,
so we denote any two-stage design with $(r_1/n_1; r/n)$ where $n = n_1 + n_2$, is the maximum sample size.
The $r_1, n_1, r$ and $n$ values are determined based on some pre-specified design parameters . 
Let $p_0$ denote the maximum unacceptable probability of response, and $p_1$ the minimum acceptable probability of response with $p_0 < p_1$.
We want to test $H_0:p≤p_0$ vs $H_1:p>p_1$ with type I error probability $α$ and power $1 − β$. These parameters $(p_0; p_1; α; β)$ are termed design parameters .

For the binomial distribution we have: $b(.;p,m)$ the probability density function and $B(.;p,m)$ the cumulative probability function, where $p$ is the probability of response and $m$ is the number of trials. So, for a two-stage design, the probability of rejecting the treatment (accepting $H_0$), is $R(p)=B(r_1;p,m)+∑^{min(r,n_1)}_{x=r_1+1} b(x;p,n_1)B(r−x;p,n_2)$. where $B(r_1;p,n_1)$ is the probability of early termination after stage one denoted by $PET(p)$. The constraints on the I type error and on the power are such as to determine many two-stage designs, denoted by $(r_1/n_1; r/n)$, satisfying the constraints

- $R(p_0)≥1−α$
- $R(p_1)≤β$
The two criteria to which we refer to select a good two-stage design are the following:

- The minmax design: which minimizes the maximum sample size, $n$, among the designs satisfying the $(α; β)$-constraint.
- The optimal design which minimizes the expected sample size $EN$ under the null hypothesis: $EN=PET(p_0) n_1 + (1−PET(p_0))n$.

## Modified Simon's Two-Stage Design

The modification is done by using the conditional probability and allows for early termination as well as extension with sample size adjustment. 
Let $X_i=1$ with probability $p$, $X_i=0$ with probability $1−p$ for $i=1,...,N$, where $N$ is the number of patients, and we consider the following hypotesys test with binary data: $H_0:p=p_0$ vs $H_1:p=p_1$ where $p_0$ is the response rate for the standard therapy and p1 is the targeted response rate for the experimental therapy under investigation.

### Fixed sample size design
First we consider the fixed sample size design without any interim analysis to obtain the maximal sample size $N$. We would like to have a power $1 − β$, and a Type I error rate $α$. Let $X_n=∑^n_{i=1} X_i$ the total number of responses out of $n$ patients where $X_n$ has a binomial distribution. The sample size $N$ and the critical boundary $R$ (such that if $X_N>R$ we reject $H_0$) have to satisfy

- $P( reject H_0 | H_0 is true ) = P(X_N>R|p=p_0) = 1−B(R−1,N,p_0)≤α$
- $1−B(R−1,N,p_1)≥1−β$
  
### Monitoring regions and sample size re-estimation
Suppose we wanto to conduct the interim analysis when $n^∗_1$ patients are accrued. If a total of $R$ responses is necessary to reject the null hypothesis at the final analysis, then we want to spend a small amount of $α$, denoted by $α_1$, for early stop for overwhelmingly strong efficacy at the interim analysis. Given $n^∗_1$ and the threshold number of responses needed for early termination of the trial for overwhelming efficacy, we are able to determine α1 and then, after that, we need to adjust R to $R^′ (R≥R^′)$. 
Suppose we observe Xn∗1 responses after the interim analysis. We calculate the conditional power under the alternative hypothesys and under the current trend:

- $CP_a=1−B(R−X_{n^∗_1}−1,N− n^∗_1,p_1)$,
- $CP_c=1−B(R−X_{n^∗_1}−1,N− n^∗_1,X_{n^∗_1}/n^∗_1)$.
  
If the conditional power:
- $0.05 < CP \le 0.9$ then the trial is in the hopeful region and it should continue;
- $CP<0.05$ the trial is hopeless and may be terminate early due to futility;
- $CP>0.9$ the results fall into the favorable region and the trial may be terminate early because is overwhelmingly positive.
  
If our trial is in the hopeful region it should continue; when continuing the trial, we may either keep the original sample size $N$ or increase the sample size beyond $N$ (denoted with $N^s$) to enhance the power at a desired level depending on the observed response rate at interim. 
To re-estimate the sample size the new values of $N^s$ and $R^s$ must satisfy these two conditions:

- $CP_0(N^s,R^s)≤CP_0(N,R)$, where $CP_0$ is the conditional power under $H_0$;
- $CP_0(N^s,R^s)≥CP_{required}$, where $CP_{required}$ is a desired level of conditional power.
  
All solutions for $N^s$ and $R^s$ that satisfy the condition above are feasible solutions. The optimal solution ($N^s$, $R^s$) is the one where $N^s$ is the smallest new sample size among all the feasible solutions.
