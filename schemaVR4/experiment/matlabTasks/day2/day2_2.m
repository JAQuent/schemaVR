% Function to copy
%questionnaire('instructions_questionnaire.txt', 'questions.txt', subNum, 'data/questionnaire')

% Runs script for schemaVR4 on day 2
date              = datestr(now,'yyyymmdd');
startTime         = datestr(now,'HHMM');

disp('Start experiment');
KbWait;  
    
schemaVR4_3AFC_day2(subNum, setNum);
schemaVR4_ratingTask('instructionsObjectLocation.txt',...
           strcat('questionsObjectLocation_',num2str(setNum), '.txt'),....
           subNum,...
           'ratingObjLoc_day2');
schemaVR4_ratingTask('instructionsGeneral.txt',...
           'questionsGeneral.txt',....
           subNum,...
           'ratingGen_day2');

% Save demographic file
endTime         = datestr(now,'HHMM');
datafilename    = strcat('data/demographic_day2_' ,num2str(subNum),'.txt');
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