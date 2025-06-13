exampleMap1 = struct();
exampleMap1.map = [
  '######';
  '#@ $.#';
  '######'
];

exampleMap2 = struct();
exampleMap2.map = [
    '######';
    '# $ .#';
    '#@   #';
    '######'
];

exampleMap3 = struct();
exampleMap3.map = [
    '#########';
    '#.$@$  .#';
    '#########'
];

clc;

function newData = getInitialSolutionSokoban(data)

    newData = data;
    map = data.map;
    
    movements = {[0, -1], [0,  1], [-1, 0], [1, 0]};

    % FIND PLAYER
    [row, col] = find(map == '@');
    if isempty(row)
        [row, col] = find(map == '+');
    end

    % GENERATE MOVEMENT
    mov = movements{randi(4)};
    newRow = row + mov(1);
    newCol = col + mov(2);

    if newRow < 1 || newRow > size(map,1) || newCol < 1 || newCol > size(map,2)
        return
    end

    newPos = map(newRow, newCol);

    % INVALID MOVE CASE
    if newPos == '#'
        return
    end

    % PUSH CASE
    if newPos == '$' || newPos == '*'
        checkRow = newRow + mov(1);
        checkCol = newCol + mov(2);

        if checkRow < 1 || checkRow > size(map,1) || checkCol < 1 || checkCol > size(map,2)
            return
        end

        % nova posição
        checkPos = map(checkRow, checkCol);

        % INVALID PUSH CASE
        if checkPos == '#' || checkPos == '$' || checkPos == '*'
            return
        end

        % MOVE BOX
        if checkPos == '.'
            map(checkRow, checkCol) = '*';
        else
            map(checkRow, checkCol) = '$';
        end

        % MOVE PLAYER
        if newPos == '*'
            map(newRow, newCol) = '+';
        else
            map(newRow, newCol) = '@';
        end

        if map(row, col) == '+'
            map(row, col) = '.';
        else
            map(row, col) = ' ';
        end

        newData.map = map;

        

        return
    end

    % BASE MOVE CASE
    if newPos == '.' 
        map(newRow, newCol) = '+';
    else
        map(newRow, newCol) = '@';
    end

    if map(row, col) == '+'
        map(row, col) = '.';
    else
        map(row, col) = ' ';
    end

    newData.map = map;
end

function score = evalFunc(state)
    map = state.map;

    [goalRows, goalCols] = find(map == '.' | map == '+');
    [boxRows, boxCols] = find(map == '$');

    boxesOnGoals = sum(map(:) == '*');

    totalBoxDistance = 0;
    for i = 1:length(boxRows)
        boxPos = [boxRows(i), boxCols(i)];

        minDist = inf;
        for j = 1:length(goalRows)
            goalPos = [goalRows(j), goalCols(j)];
            dist = abs(boxPos(1) - goalPos(1)) + abs(boxPos(2) - goalPos(2));
            if dist < minDist
                minDist = dist;
            end
        end

        totalBoxDistance = totalBoxDistance + minDist;
    end

    deadlockPenalty = 0;
    for i = 1:length(boxRows)
        r = boxRows(i);
        c = boxCols(i);

        if map(r, c) ~= '.'
            if (map(r-1, c) == '#' && map(r, c-1) == '#') || ...
               (map(r-1, c) == '#' && map(r, c+1) == '#') || ...
               (map(r+1, c) == '#' && map(r, c-1) == '#') || ...
               (map(r+1, c) == '#' && map(r, c+1) == '#')
                deadlockPenalty = deadlockPenalty + 10000;
            end
        end
    end

    score = -totalBoxDistance + 10 * boxesOnGoals - deadlockPenalty;
end

function neighbor = getRandomNeigh(state)
    % Tenta gerar um vizinho válido a partir do estado atual
    maxTries = 50;  % evitar ciclos infinitos
    for i = 1:maxTries
        neighbor = getInitialSolutionSokoban(state);  % aplica movimento aleatório

        if ~isequal(neighbor.map, state.map)
            return  % retorno se foi gerado um estado diferente (válido)
        end
    end

    % Se não conseguir gerar vizinho válido, retorna o mesmo (pode melhorar isso)
    neighbor = state;
end

function x = p(fu, fv, T, sense)
     if strcmp(sense, 'max')
        % Maximization problem
        x = exp((fv-fu) / (T*fu));
        disp(x);
    elseif strcmp(sense, 'minx')
        % Minimization problem
        x = exp((fu-fv) / (T*fu));
        disp(x);
    end
end

function r = myRand()
    r = rand();
end

function bool = isOptimum(state)
    map = state.map;
    
    boxesOutsideGoals = sum(map(:) == '$');
    
    if boxesOutsideGoals ~= 0
        bool = false;
    else
        bool = true;
    end
end

function newtemp = Temp(t, Tmax, R)
%Outra hipotese: decrescimento quadratico
% R = ]0,1[, e.g., R = 0.99 (mais lento) ou 0.1 (mais rapido)
% newTemp = R*Tmax;
    newtemp = Tmax * exp(-R * t);
end

function Res = SA(Tmax, Tmin, R, k, ...
    data, getInitialSolution, getRandomNeigh, evalFunc, ...
    isOptimum, sense)

    t = 0;
    T = Tmax;
    numEvaluations = 0;
    foundOptimum = false;

    maxTries = 100;
    for attempt = 1:maxTries
        u = getInitialSolution(data);
        if ~isequal(u.map, data.map)
            break;
        end
    end
    fu = evalFunc(u);
    fprintf('Initial cost: %d\n', fu);
    numEvaluations = numEvaluations + 1;

    z = 1;
    F(z) = fu;
    trajectory{z} = u.map;
    z = z+1;

    while (~foundOptimum)
        i = 0;
        while (i < k && ~foundOptimum)
            v = getRandomNeigh(u);
            fv = evalFunc(v);
            disp(v.map);
            numEvaluations = numEvaluations + 1;

            dif = fv - fu;
            if (strcmp(sense, 'max'))
                dif = -dif;
            end

            fprintf('fu (sol) = %d, fv (new neighbour) = %d\n', fu, fv);

            accepted = false;
            if (dif < 0)
                disp('Neighbour accepted')
                accepted = true;
            else
                prob = p(fu, fv, T, sense);
                x = myRand();
                if (x <= prob)
                    disp('Neighbour accepted with probability');
                    accepted = true;
                end
            end

            if accepted
                u = v;
                fu = fv;
                trajectory{z} = u.map;  % <--- Salva o estado aceito
                F(z) = fu;
                z = z+1;
            end

            i = i + 1;

            if isOptimum(u)
                foundOptimum = true;
            end
        end

        if ~foundOptimum
            t = t + 1;
            T = Temp(t, Tmax, R);
            if (T < Tmin)
                break;
            end
        end
    end

    disp('BestCost: '); 
    disp(fu);

    disp('numEvaluations: '); 
    disp(numEvaluations);

    Res = struct('T', T, ...
                 'NumEvaluations', numEvaluations, ...
                 'Cost', fu, ...
                 'Tmax', Tmax, ...
                 'Tmin', Tmin, ...
                 'R', R, ...
                 'k', k, ...
                 'u', u.map, ...
                 'F', F, ...
                 's', u, ...
                 'trajectory', {trajectory});  % <--- Retorna o caminho

    figure(1);
    plot(F);
end


Tmax = 100;
Tmin = 1e-3;
R = 0.95;
k = 16;

Res = SA(Tmax, Tmin, R, k, ...
    exampleMap2, ...
    @getInitialSolutionSokoban, ...
    @getRandomNeigh, ...
    @evalFunc, ...
    @isOptimum, ...
    'max');
for i = 1:length(Res.trajectory)
    disp(['Step ', num2str(i)]);
    disp(Res.trajectory{i});
    pause(0.5);
end
