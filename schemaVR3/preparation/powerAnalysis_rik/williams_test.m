function [t,df] = williams_test(S);

% Williams test for correlated correlations
% rik.henson@mrc-cbu.cam.ac.uk, June 2016

if isfield(S,'X')
    allR = corrcoef(S.X);
    N    = size(S.X,1);
elseif isfield(S,'R') & isfield(S,'N')
    allR = S.R;
    N    = S.N;
else
    error('Either pass S.X = Nx3 data matrix, or S.R = 3x3 corrrelation matrix and S.N = N')
end

% https://en.wikipedia.org/wiki/Fisher_transformation
allZ = atanh(allR);


%% Below from http://psych.unl.edu/psycrs/statpage/biv_corr_comp_eg.pdf
% According to above PDF, result is a Z-value, but I think that is because
% their example has larg N - assume below that result is T-value...

rm2 = (allR(1,2)^2 + allR(1,3)^2) / 2;

f   = (1 - allR(2,3)) / (2 * (1 - rm2));

h   = (1 - f*rm2) / (1 - rm2);

Z   = (allZ(1,2) - allZ(1,3)) * sqrt( (N-3) / (2 * (1-allR(2,3)) * h) );

t = Z; df = N-3;
