d = readtable('rikData.csv');

objs = unique(d.objNum);

nr=[];pr=[];
for s = 1:length(objs)
    
    N    = size(d(find(strcmp(d.objNum,objs{s})),:),1);
    er   = table2array(d(find(strcmp(d.objNum,objs{s}) & d.subNum>=25 & strcmp(d.recollection,'recollected')),'expectancy'));
    enr  = table2array(d(find(strcmp(d.objNum,objs{s}) & d.subNum>=25 & ~strcmp(d.recollection,'recollected')),'expectancy'));    
    
    if length(er)>0 
       nr(end+1,:) = [mean(er) mean(enr)];
       pr(end+1,:) = [length(er)/N mean(er)];
    end
end
figure,boxplot(nr)
mean(nr)


[R,p]=corr(pr(:,1),pr(:,2))
figure,plot(pr(:,2),pr(:,1),'o')

% Fit linear model to Exp2 data
y = pr(:,1);
X = [pr(:,2) ones(size(pr(:,2)))];
B = pinv(X)*y; 
r = y - X*B;
sd = std(r);

% Just check get same p-value as Pearson (yup)
%df = length(y) - rank(X);
%s = r'*r / df;
%c = [-1 0]'; 
%sB = sqrt(s*c'*pinv(X'*X)*c);
%2*t2p(c'*B/sB,df)

% “Me” below is mean expectancy across subjects for each object, calculated from Exp2 post-ratings

Nsims = 1000;
Nobjs = length(objs)
Nsubs = length(subs)
%Nsubs = 10;
R=[];
for n=1:Nsims
    for o = 1:Nobjs
        for s = 1:Nsubs
            p = [Me(o) 1] * B + randn(1)*sd; % not really a probability since can be <0,>1…
            r(o,s) = rand(1) <= p;
        end
    end
    pR = mean(r,2);
    R(n) = corr(pR,Me');
end

t = R.*sqrt((Nobjs-2)./(1-R.^2)); 
p = tcdf(t,Nobjs-2); % one-tailed for negative slope
power = length(find(p<.05))/Nsims
