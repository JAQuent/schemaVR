
clear

% cd /home/rh01/PC/Collaborations/AlexQ
%cd U:/PC/Collaborations/AlexQ
d = readtable('rikData.csv');

%d.Properties.VariableNames

%% Correlation between pR and obj post-ratings
objs = unique(d.objNum);
nr=[];pr=[];Me=[];nk=[];pk=[];
for s = 1:length(objs)
    
    e    = table2array(d(find(strcmp(d.objNum,objs{s})),{'expectancy'}));
    N    = length(e); Me(s) = mean(e);
    er   = table2array(d(find(strcmp(d.objNum,objs{s}) & strcmp(d.recollection,'recollected')),'expectancy'));
    enr  = table2array(d(find(strcmp(d.objNum,objs{s}) & ~strcmp(d.recollection,'recollected')),'expectancy'));
    
%    Nk   = N;
    Nk   = length(find(strcmp(d.objNum,objs{s}) & ~strcmp(d.recollection,'recollected'))); % independence
    ek   = table2array(d(find(strcmp(d.objNum,objs{s}) & strcmp(d.familiar,'familiar')),'expectancy'));
    enk  = table2array(d(find(strcmp(d.objNum,objs{s}) & ~strcmp(d.familiar,'familiar') & ~strcmp(d.recollection,'recollected')),'expectancy'));

%     Nk   = size(d(find(strcmp(d.objNum,objs{s}) & strcmp(d.kitchen,'kitchen')),:),1);
%     erk  = table2array(d(find(strcmp(d.objNum,objs{s}) & strcmp(d.recollection,'recollected') & strcmp(d.kitchen,'kitchen')),'expectancy'));
%     enrk = table2array(d(find(strcmp(d.objNum,objs{s}) & ~strcmp(d.recollection,'recollected') & strcmp(d.kitchen,'kitchen')),'expectancy'));
    
    
    if length(er)>0 
       nr(end+1,:) = [mean(er) mean(enr)];
%       pr(end+1,:) = [length(er)/N mean(er)];
       pr(end+1,1:2) = [length(er)/N Me(s)];
    end
    if length(ek)>0 
       nk(end+1,:) = [mean(ek) mean(enk)];
%       pr(end+1,:) = [length(er)/N mean(er)];
       pk(end+1,:) = [length(ek)/Nk Me(s)];
    end

%     if length(erk)>0
%         nr(end+1,:) = [mean(erk) mean(enrk)];
%         pr(end+1,:) = [length(erk)/Nk mean(erk)];
%     end
end

figure,boxplot(nr)
mean(nr)
mean(nk)
[R,p]=corr(pr(:,1),pr(:,2))
figure,plot(pr(:,2),pr(:,1),'o')
[R,p]=corr(pk(:,1),pk(:,2))
figure,plot(pk(:,2),pk(:,1),'o')

S.X = [pr(:,2) pr(:,1) pk(:,1)];
[wt,df] = williams_test(S);
t2p(-wt,df)


%% Fit a linear regression
y = pr(:,1);
X = [pr(:,2) ones(size(pr(:,2)))];
B = pinv(X)*y; 
r = y - X*B;
sd = std(r);

% Just check same p-value
% df = length(y) - rank(X);
% s = r'*r / df;
% c = [-1 0]'; sB = sqrt(s*c'*pinv(X'*X)*c);
% 2*t2p(c'*B/sB,df)

y = pk(:,1);
X = [pk(:,2) ones(size(pk(:,2)))];
Bk = pinv(X)*y; 
r = y - X*B;
sdk = std(r);


%% Switch to norm obj ratings
% d0 = readtable('schemaVR2_ratings.csv');
% objs0 = d0.objNam;
% objs{1} = 'fruitBowl';
% objs{6} = 'glassContainer';
% objs{9} = 'kitchenRoll';
% objs{14} = 'bookPile';
% objs{16} = 'teaPot';
% objs{18} = 'towel';
% for o = 1:length(objs)
%     i = find(strcmp(objs0,objs{o}));
%     if isempty(i)
%         error(num2str(o))
%     else
%         pr(o,3) = str2num(table2array(d0.expectancy(i)));
%     end
% end
% corr(pr(:,2),pr(:,3)) % no correlation?
% figure,plot(pr(:,2),pr(:,3),'o')
% 
% [R,p]=corr(pr(:,1),pr(:,3))
% figure,plot(pr(:,3),pr(:,1),'o')
% 
% Uncomment if want to power based on norm ratings
% y = pr(:,1);
% X = [pr(:,3) ones(size(pr(:,3)))];
% B = pinv(X)*y; 
% r = y - X*B;
% df = length(y) - rank(X);
% sd = std(r);


%% Exp 3

nd = readtable('ratingsForSets.csv');

Nsims = 1000;
sets  = unique(nd.set);
Nsubs = 5;
R=zeros(Nsims,1); wt = zeros(Nsims,1);
for n=1:Nsims
    mE = {}; pR = {}; pK = {};
    for q = 1:length(sets)
        objs  = find(nd.set == sets(q));
        mE{q} = str2num(strvcat(table2array(nd(objs,{'expectancy'}))));       
        
        p     = repmat([mE{q} ones(size(mE{q}))]*B, 1, Nsubs) + randn(length(objs),Nsubs)*sd;
        pR{q} = mean((rand(length(objs),Nsubs)<=p),2);
        
        p     = repmat([mE{q} ones(size(mE{q}))]*Bk, 1, Nsubs) + randn(length(objs),Nsubs)*sdk;
        pK{q} = mean((rand(length(objs),Nsubs)<=p),2);     
    end
    
    mE = cat(1,mE{:}); 
    pR = cat(1,pR{:});
    
    [~,include] = unique(mE);  % remove unique values
%    include = 1:length(mE);
    
    R(n) = corr(pR(include),mE(include));
    
    S.X = [mE pR cat(1,pK{:})];
    [wt(n),df] = williams_test(S);
end

Nrep = size(mE(include),1)

t = R.*sqrt((Nrep-2)./(1-R.^2)); % +/- Inf where rho == 1
p = tcdf(t,Nrep-2); % one-tailed for negative slope

power = length(find(p<.05))/Nsims

p = tcdf(wt,Nrep-2);

power = length(find(p<.05))/Nsims


return 

%%% Sub

subs = unique(d.subNum);

nr=[];pr=[];
for s = 1:length(subs)
    
    N    = size(d(find(d.subNum==subs(s)),:),1);
    er   = table2array(d(find(d.subNum==subs(s) & strcmp(d.recollect,'recollected')),'expectancy'));
    enr  = table2array(d(find(d.subNum==subs(s) & ~strcmp(d.recollect,'recollected')),'expectancy'));

    Nk   = size(d(find(d.subNum==subs(s)  & strcmp(d.kitchen,'kitchen')),:),1);
    erk  = table2array(d(find(d.subNum==subs(s) & strcmp(d.recollect,'recollected') & strcmp(d.kitchen,'kitchen')),'expectancy'));
    enrk = table2array(d(find(d.subNum==subs(s) & ~strcmp(d.recollect,'recollected') & strcmp(d.kitchen,'kitchen')),'expectancy'));
    
    
    if length(er)>0 
       nr(end+1,:) = [mean(er) mean(enr)];
       pr(end+1,:) = [length(er)/N mean(er)];
    end
%     if length(erk)>0
%         nr(end+1,:) = [mean(erk) mean(enrk)];
%         pr(end+1,:) = [length(erk)/Nk mean(erk)];
%     end
end
figure,boxplot(nr)
mean(nr)
[R,p]=corr(pr(:,1),pr(:,2))
figure,plot(pr(:,2),pr(:,1),'o')