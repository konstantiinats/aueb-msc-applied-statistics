# AUEB M.Sc. in Applied Statistics
## Course: Financial Mathematics using Python/Matlab
## Summer 2020
## Lecturer: A. Yannakopoulos
## Author: Konstantina Tsami


### Implementation of the Vasicek model on short rate. The dataset used is the three-month EURIBOR from 02/07/2001 - 04/24/2006. 
### The aformentioned model had been used in five different cases in order to predict the EURIBOR rate return for the next 70 days.

###Language: Matlab





%Data loading
load Data_GlobalIdx2;
plot(dates, 100 * Dataset.EB3M)
datetick('x'), xlabel('Date'), ylabel('Daily Yield (%)')
title('3-Month Euribor as a Daily Effective Yield')
 
%Model fitting to data
yields     = Dataset.EB3M;
regressors = [ones(length(yields) - 1, 1) yields(1:end-1)];
[coefficients, intervals, residuals] = ...
   regress(diff(yields), regressors);
dt    = 1;  % time increment = 1 day
speed = -coefficients(2)/dt;
level = -coefficients(1)/coefficients(2);
sigma =  std(residuals)/sqrt(dt);
 
%Create an object and identify Start State (the most recent short rate)
obj = hwv(speed, level, sigma, 'StartState', yields(end))
 
%Model simulation
T      = 64;
times  = (1:T)';
t      = NaN(length(times) + 1, 1);
t(1)   = obj.StartTime;
t(2)   = T;
delta  = T;
jMax   = 1;
iCount = 3;
 
for k = 1:log2(T)
    i = delta / 2;
    for j = 1:jMax
        t(iCount) = times(i);
        i         = i + delta;
        iCount    = iCount + 1;
    end
    jMax  = 2 * jMax;
    delta = delta / 2;
end
 
%Interpolation times plot
stem(1:length(t), t, 'filled')
xlabel('Index'), ylabel('Interpolation Time (Days)')
title ('Sampling Scheme for the Power-of-Two Algorithm')
 
%Time series grid initialization
average = obj.StartState * exp(-speed * T) + level * ...
(1 - exp(-speed * T));
X       = [obj.StartState ; average];
 
%Sample paths generation
nTrials = 5;
rng(63349,'twister')
Y = obj.interpolate(t, X(:,:,ones(nTrials,1)), ...
'Times',[obj.StartTime  T], 'Refine', true);
 
%Sample paths plot
[t,i] = sort(t);
Y     = squeeze(Y);
Y     = Y(i,:);
plot(t, 100 * Y), hold('on')
plot(t([1 end]), 100 * Y([1 end],1),'. black','MarkerSize',20)
xlabel('Interpolation Time (Days into the Future)')
ylabel('Yield (%)'), hold('off')
title ('Euribor Yields from the Vasicek Model')



