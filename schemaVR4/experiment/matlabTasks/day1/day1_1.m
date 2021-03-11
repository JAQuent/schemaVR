% This script runs all tasks for noveltyVR and schemaVR4
% Get demographics
VR                = input('Have you used VR before? ');
subNum            = input('Subject number: ');
age               = input('Age in years: ');
cond              = input('Condition: ');
group             = input('Group: ');
setNum            = input('Set number: ');
disp('Gender:');
disp('0 for female');
disp('1 for male');
disp('2 for non-binary');
gender            = input('Indicate gender: ');
if gender == 0
    gender = 'female';
elseif gender == 1
    gender = 'male';
else
    gender = 'non-binary';
end
date      = datestr(now,'yyyymmdd');
startTime = datestr(now,'HHMM');

date              = datestr(now,'yyyymmdd');
startTime         = datestr(now,'HHMM');


disp('Start experiment');
KbWait;
    
schemaVR4_3AFC_day1(subNum, setNum);
schemaVR4_ratingTask('instructionsObjectLocation.txt',...
           strcat('questionsObjectLocation_',num2str(setNum), '.txt'),....
           subNum,...
           'ratingObjLoc_day1');
schemaVR4_ratingTask('instructionsGeneral.txt',...
           'questionsGeneral.txt',....
           subNum,...
           'ratingGen_day1');

% Save demographic file
endTime         = datestr(now,'HHMM');
datafilename    = strcat('data/demographic_day1_' ,num2str(subNum),'.txt');
datafilepointer = fopen(datafilename,'wt'); % opens ASCII file for writing
fprintf(datafilepointer,'%i %s %i %s %s %s\n', ...
    subNum,...
    setNum,...
    gender,...
    age,...
    date,...
    startTime,...
    endTime);
fclose('all');