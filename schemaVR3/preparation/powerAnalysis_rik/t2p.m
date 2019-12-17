function p=t2p(t,df,tf)

if nargin < 3
    tf = 1; % tail-flag, ie one-tailed by default (assumes sign of t matches direct of hypothesis)
end

try
    p = spm_Tcdf(t,df);
catch
    p = tcdf(t,df);
end

if tf == 1
    fprintf('One-tailed: Assuming sign of t matches direction of hypothesis!\n')
    p = 1 - p;
else
    f = find(p>0.5);
    p(f) = (1 - p(f));
    p = p * 2;
end

