

[subNoUsed, ~, ~] = textread('log.txt','%n %s %s', 'delimiter','\t');
subNum            = input('Subject number: ');
setNum            = input('Set number: ');
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
        subNum     = input('Subject number: ');
        date       = datestr(now,'yyyymmdd');
        setNum     = input('Set number: ');
        startTime  = datestr(now,'HHMM');
    end
end

disp('Start experiment');
KbWait;
    
schemaVR3_3AFC(subNum, setNum);
schemaVR3_ratingTask('instructionsObjectLocation.txt',...
           strcat('questionsObjectLocation_',num2str(setNum), '.txt'),....
           subNum,...
           'ratingObjLoc');
schemaVR3_ratingTask('instructionsGeneral.txt',...
           'questionsGeneral.txt',....
           subNum,...
           'ratingGen');

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
    setNum,...
    gender,...
    age,...
    date,...
    startTime,...
    endTime);
fclose('all');