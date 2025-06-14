function Test_Sokoban_GA()
    %
    % OneMax simple GA (string size = n)
    % 

    clc; % Clear screen

    dataBoard = struct();
    dataBoard.map = [
    '######';
    '# $ .#';
    '#@   #';
    '######'
    ];
    
    % String size = N
    N = 5;
    % Optimum = 200 = sum(max number of ones)
    optimum = 0;
    % Create struct data that will contain problem data
    data = struct('N', N, 'optimum', optimum, 'dataBoard', dataBoard);

    % Max number of iterations (generations)
    tmax      = 2000;
    % Population size
    popSize   = 100;
    % Cross and mutation probability
    crossProb = 0.5;
    mutProb   = 0.3; % change

    % Run Tests
    NumbOfTests = length(tmax);  
    for f = 1 : NumbOfTests 
        fprintf('\ntmax=%d', tmax(f));
        fprintf('\npopSize=%d', popSize(f));
        fprintf('\ncrossProb=%.1f', crossProb(f));
        fprintf('\nmutProb=%.1f\n', mutProb(f));
        
        fprintf('\nNumEvaluations\t\tCost\n');
        fprintf('============================\n');
        % GA algorithm
        Results = Sokoban_GA(data, tmax(f), popSize(f), crossProb(f), ...
             mutProb(f));
        
        Res = [[Results.NumEvaluations]; [Results.Cost]];
        fprintf('%d\t\t%d\n', Res);
    end
end
                    
Test_Sokoban_GA();