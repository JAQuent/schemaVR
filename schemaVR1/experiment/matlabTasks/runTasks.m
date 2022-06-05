% This script runs the matlab task for schemaVR1

[subNoUsed, ~, ~] = textread('log.txt','%n %s %s', 'delimiter','\t');
subNum             = input('Subject number: ');
age               = input('Age in years: ');
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
date              = datestr(now,'yyyymmdd');
startTime         = datestr(now,'HHMM');

if subNum ~= 0
    while any(subNum == subNoUsed)
        disp('Subject number already choosen:')
        subNum = input('Subject number: ');
        date  = datestr(now,'yyyymmdd');
        startTime  = datestr(now,'HHMM');
    end
end

disp('Start experiment');
KbWait;
    
retrievalTask1(subNum);
ratingTask('ratingTask/stimuli/instructionsObjectLocation.txt',...
           'ratingTask/stimuli/questionsObjectLocation.txt',....
           subNum,...
           'ratingObjectLocation');
ratingTask('ratingTask/stimuli/instructionsGeneral.txt',...
           'ratingTask/stimuli/questionsGeneral.txt',....
           subNum,...
           'ratingGeneral');

% Update log file
logFile = fopen('log.txt','a+');
fprintf(logFile,'%i %s %s\n', ...
    subNum,...
    date,...
    startTime);
fclose('all');

% Save demographic file
endTime         = datestr(now,'HHMM');
datafilename    = strcat('data/demographic_' ,num2str(subNum),'.txt');
datafilepointer = fopen(datafilename,'wt'); % opens ASCII file for writing
fprintf(datafilepointer,'%i %s %i %s %s %s\n', ...
    subNum,...
    gender,...
    age,...
    date,...
    startTime,...
    endTime);
fclose('all');