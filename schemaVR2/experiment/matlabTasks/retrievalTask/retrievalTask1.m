function [ ] = retrievalTask1(subNo)
%retrievalTask1 
% % % % % % % % % % % % % % % % % % % % % % % % % 
% 3AFC recogntion location task 
% Author: Alexander Quent (alex.quent at mrc-cbu.cam.ac.uk
% Version: 2.0
% % % % % % % % % % % % % % % % % % % % % % % % %

%% Explanations

try
%% Setting everuthing up
    % Preliminary stuff
    % Clear Matlab/Octave window:
    clc;

    % check for Opengl compatibility, abort otherwise:
    AssertOpenGL;

    % Reseed randomization
    rand('state', sum(100*clock));

    % General information about subject and session
    date  = str2num(datestr(now,'yyyymmdd'));
    time  = str2num(datestr(now,'HHMMSS'));

    % Get information about the screen and set general things
    Screen('Preference', 'SuppressAllWarnings',0);
    Screen('Preference', 'SkipSyncTests', 1);
    screens       = Screen('Screens');
    if length(screens) > 1
        error('Multi display mode not supported.');
    end
    rect             = Screen('Rect',0);
    refreshRate      = 1/Screen('FrameRate', 0); % refresh rate in secs
    center           = round([rect(3) rect(4)]/2);
    questionPosition = 0.2;
    keyPosition      = 0.8;

    % RGB Colors 
    bgColor    = [255, 255, 255];
    fixColor   = [0, 0, 0];

    % Relevant key codes
    KbName('UnifyKeyNames');
    space  = KbName('space');
    escape = KbName('ESCAPE');
    AFC    = [KbName('1!') KbName('2@') KbName('3#')];
    CON    = [KbName('0)') KbName('1!') KbName('2@') KbName('3#')]; 
    % numberKeys need sto be adjusted for the respective layout of the
    % keyboard.

    % Textures and text
    fixLen              = 20; % Size of fixation cross in pixel
    fixWidth            = 3;
    textSize            = [30 25];
    
    % Instruction
    lineLength    = 70;
    messageIntro1 = WrapString('Retrieval task \n\n At each trial you will see the same object in three locations and your task is to choose the correct one out of 3 alternatives by pressing number at top of the keyboard that corresponds to the picture. \n\n After that decision, please indicate how you made that decision. If you don’t remember seeing the object at all, press 0 for no memory. When you didn’t know the object was there, but you guess, press 1 for guess. If that location just looks familiar to you, now, press 2 for familiar. Another option is that you remembered that this particular item was there because you remembered it was next the sink or next to another item. In this case you press 3 for remember.\n Please press spacebar to start',lineLength);

    % Opening window and setting preferences
    try
        [myScreen, rect]    = Screen('OpenWindow', 0, bgColor);
    catch
        try
            [myScreen, rect]    = Screen('OpenWindow', 0, bgColor);
        catch
            try
                [myScreen, rect]    = Screen('OpenWindow', 0, bgColor);
            catch
                try
                    [myScreen, rect]    = Screen('OpenWindow', 0, bgColor);
                catch
                    [myScreen, rect]    = Screen('OpenWindow', 0, bgColor);
                end
            end
        end
    end
    slack       = Screen('GetFlipInterval', myScreen)/2; % Getting lack for accurate timing
    center      = round([rect(3) rect(4)]/2);
    HideCursor;

    % Output files
    datafilename = strcat('data/retrievalTask_',num2str(subNo),'.dat'); % name of data file to write to
    mSave        = strcat('data/retrievalTask_',num2str(subNo),'.mat'); % name of another data file to write to (in .mat format)
    mSaveALL     = strcat('data/retrievalTask_',num2str(subNo),'all.mat'); % name of another data file to write to (in .mat format)
    % Checking for existing result file to prevent accidentally overwriting
    % files from a previous subject/session (except for subject numbers > 0):
    datafilepointer = fopen(datafilename,'wt'); % opens ASCII file for writing

    % Creating trials
    [objectNumber, foil1Rank, encodingRank, foil2Rank, foil1Location, encodingLocation, foil2Location] = textread(strcat('finalSelection.txt'),'%n %n %n %n %n %n %n', 'delimiter','\t');
    nTrial         = length(objectNumber);
    
    % Randomizing order of trial
    shuffle          = randomOrder(nTrial, nTrial);
    objectNumber     = objectNumber(shuffle);
    encodingLocation = encodingLocation(shuffle);
    encodingRank     = encodingRank(shuffle);
    foil1Location    = foil1Location(shuffle);
    foil1Rank        = foil1Rank(shuffle);
    foil2Location    = foil2Location(shuffle);
    foil2Rank        = foil2Rank(shuffle);
    
    % Response variables
    RT             = zeros(nTrial, 2) - 99;
    responses      = zeros(nTrial, 2) - 99;
    correctness    = zeros(nTrial, 1) - 99;
    results        = cell(nTrial, 18); 
    
    % Loading stimuli
    imageLocScale            = 0.5;
    encodingLocationImages   = {};
    encodingLocationTextures = [];
    foil1Images              = {};
    foil1Textures            = [];
    foil2Images              = {};
    foil2Textures            = [];
    
    % Locations
    for i = 1:nTrial
        encodingLocationImages{i}   = imresize(imread(strcat('retrievalTask/stimuli/', num2str(objectNumber(i)),'_' , num2str(encodingLocation(i)), '.png')), imageLocScale);
        encodingLocationTextures(i) = Screen('MakeTexture', myScreen, encodingLocationImages{i});
        
        foil1Images{i}              = imresize(imread(strcat('retrievalTask/stimuli/', num2str(objectNumber(i)),'_' , num2str(foil1Location(i)), '.png')), imageLocScale);
        foil1Textures(i)            = Screen('MakeTexture', myScreen, foil1Images{i});
        
        foil2Images{i}              = imresize(imread(strcat('retrievalTask/stimuli/', num2str(objectNumber(i)),'_' , num2str(foil2Location(i)), '.png')), imageLocScale);
        foil2Textures(i)            = Screen('MakeTexture', myScreen, foil2Images{i});
    end
    imageLocSize   = size(encodingLocationImages{1});
    shift          = 50;
    leftPosition   = [center(1) - imageLocSize(2)*1.5 - shift, center(2) - imageLocSize(1)/2, center(1) - imageLocSize(2)*0.5 - shift, center(2) + imageLocSize(1)/2];
    middlePosition = [center(1) - imageLocSize(2)*0.5, center(2) - imageLocSize(1)/2, center(1) + imageLocSize(2)*0.5, center(2) + imageLocSize(1)/2];
    rightPosition  = [center(1) + imageLocSize(2)*0.5 + shift, center(2) - imageLocSize(1)/2, center(1) + imageLocSize(2)*1.5 + shift, center(2) + imageLocSize(1)/2];

%% Experimental loop
    for trial = 1:nTrial
        % Exercise and instruction
        if trial == 1
            Screen('TextSize', myScreen, textSize(2)); % Sets size to instruction size
            % Page 1
            DrawFormattedText(myScreen, messageIntro1, 'center', 'center');
            Screen('Flip', myScreen);
            KbReleaseWait;
            [~, ~, keyCode] = KbCheck; 
            while keyCode(space) == 0 
                [~, ~, keyCode] = KbCheck;
            end
            
            % Specifiying font settings for trials
            Screen('TextColor', myScreen, [0 0 0]); % Sets to normal font color
            Screen('TextFont', myScreen, 'DejaVu'); % Sets normal font
            Screen('TextSize', myScreen, textSize(1)); % Sets size to normal
        end
        %% Fixation cross
        Screen('DrawLine', myScreen, fixColor, center(1)- fixLen, center(2), center(1)+ fixLen, center(2), fixWidth);
        Screen('DrawLine', myScreen, fixColor, center(1), center(2)- fixLen, center(1), center(2)+ fixLen, fixWidth);
        Screen('Flip', myScreen);
        
        
        %% Location presentation for 3AFC
        posShuffle = randomOrder(3, 3);
        AFCloc = [encodingLocationTextures(trial) foil1Textures(trial) foil2Textures(trial)];
        AFCloc = AFCloc(posShuffle);
        
        Screen('DrawTexture', myScreen,  AFCloc(1), [], leftPosition);
        DrawFormattedText(myScreen, '1', (leftPosition(3) + leftPosition(1))/2, rect(4)*keyPosition);
        left = posShuffle(1);
        
        Screen('DrawTexture', myScreen,  AFCloc(2), [], middlePosition);
        DrawFormattedText(myScreen, '2', (middlePosition(3) + middlePosition(1))/2, rect(4)*keyPosition);
        middle = posShuffle(2);
        
        Screen('DrawTexture', myScreen,  AFCloc(3), [], rightPosition);
        DrawFormattedText(myScreen, '3', (rightPosition(3) + rightPosition(1))/2, rect(4)*keyPosition);
        right = posShuffle(3);
        
        AFCOnset = Screen('Flip',myScreen);

        % 3AFC
        [~, secs, keyCode] = KbCheck; % saves whether a key has been pressed, seconds and the key which has been pressed.
        while keyCode(AFC(1)) == 0 && keyCode(AFC(2)) == 0 && keyCode(AFC(3)) == 0
            [~, secs, keyCode] = KbCheck;
            % No criteria for skipping response
        end
        AFCOffest     = Screen('Flip',myScreen);
        AFCPreTime    = (AFCOffest - AFCOnset)*1000;
        RT(trial, 1)  = (secs - AFCOnset)*1000;
        KbReleaseWait;

        % Response coding 3AFC
        if keyCode(AFC(1)) == 1
            responses(trial, 1) = 1;
        elseif keyCode(AFC(2)) == 1
            responses(trial, 1) = 2;
        elseif keyCode(AFC(3)) == 1
            responses(trial, 1) = 3; 
        end

        % Checking accuracy 3AFC
        if encodingLocationTextures(trial) == AFCloc(responses(trial, 1))
            correctness(trial, 1) = 1;
        else
            correctness(trial, 1) = 0;
        end

        %% Confidence rating
        DrawFormattedText(myScreen, '0 = no memory |  1 = guess \n 2 = familiar | 3 = remember', 'center', 'center');
        conOnset = Screen('Flip', myScreen);
        [~, secs, keyCode] = KbCheck; % saves whether a key has been pressed, seconds and the key which has been pressed.
        while keyCode(CON(1)) == 0 && keyCode(CON(2)) == 0 && keyCode(CON(3)) == 0 && keyCode(CON(4)) == 0
            [~, secs, keyCode] = KbCheck;
        end
        conOffset    = Screen('Flip', myScreen);
        conPreTime   = (conOffset - conOnset)*1000;
        RT(trial, 2) = (secs - conOnset)*1000;
        KbReleaseWait;

        % Coding confidence response
        if keyCode(CON(1)) == 1
            responses(trial, 2) = 0;
        elseif keyCode(CON(2)) == 1
            responses(trial, 2) = 3;
        elseif keyCode(CON(3)) == 1
            responses(trial, 2) = 2;
        elseif keyCode(CON(4)) == 1
            responses(trial, 2) = 1;
        end

        %% Saving data
        fprintf(datafilepointer,'%i %i %i %i %i %i %f %i %f %i %f %f %f %i %i %f %f %i %i %i %i\n', ...
            subNo,...
            date,...
            time,...
            trial,...
            objectNumber(trial),...
            encodingLocation(trial),...
            encodingRank(trial),...
            foil1Location(trial),...
            foil1Rank(trial),...
            foil2Location(trial),...
            foil2Rank(trial),...
            AFCPreTime,...
            RT(trial, 1),...
            responses(trial, 1),...
            correctness(trial, 1),...
            conPreTime,...
            RT(trial, 2),...
            responses(trial, 2),...
            left,...
            middle,...
            right);

        % Save everything in a varibles that is saved at the end.
        % subNo, date, time, 
        results{trial, 1}  = subNo;
        results{trial, 2}  = date;
        results{trial, 3}  = time;
        results{trial, 4}  = trial;
        results{trial, 5}  = objectNumber(trial);
        results{trial, 6}  = encodingLocation(trial);
        results{trial, 7}  = encodingRank(trial);
        results{trial, 8}  = foil1Location(trial);
        results{trial, 9}  = foil1Rank(trial);
        results{trial, 10} = foil2Location(trial);
        results{trial, 11} = foil2Rank(trial);
        results{trial, 12} = AFCPreTime;
        results{trial, 13} = RT(trial, 1);
        results{trial, 14} = responses(trial, 1);
        results{trial, 15} = correctness(trial, 1);
        results{trial, 16} = conPreTime;
        results{trial, 17} = RT(trial, 2);
        results{trial, 18} = responses(trial, 2);
        results{trial, 19} = left;
        results{trial, 20} = middle;
        results{trial, 21} = right;
    end
    %% End of experiment
    % Saving .m files and closing files
    save(mSave, 'results');
    save(mSaveALL);
    fclose('all');

    % End Screen
    Screen('TextColor', myScreen, [0 0 0]); % Sets to normal font color
    Screen('TextFont', myScreen, 'DejaVu'); % Sets normal font
    Screen('TextSize', myScreen, textSize(2)); % Sets size to instruction size
    DrawFormattedText(myScreen, horzcat('End of this part. \n Please press escape to go to the next part.'), 'center', 'center');
    Screen('Flip', myScreen);
    [~, ~, keyCode] = KbCheck; 
    while keyCode(escape) == 0 
        [~, ~, keyCode] = KbCheck;
    end

    Screen('CloseAll')
    ShowCursor;
catch
    ShowCursor;
    fclose('all');
    % Saving .m files and closing files
    save(mSave, 'results');
    save(mSaveALL);
    Screen('CloseAll')
    rethrow(lasterror)
end
end

