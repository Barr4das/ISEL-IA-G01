function c = evalFunction(s, data)
    map = data.map;

    [goalRows, goalCols] = find(map == '.' | map == '+');
    [boxRows, boxCols] = find(map == '$');

    boxesOnGoals = sum(map(:) == '*');

    boxesNotOnGoals = sum(map(:) == '$');

    scoreBoxes = 10 * (boxesNotOnGoals - boxesOnGoals);

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
                deadlockPenalty = deadlockPenalty + 10;
            end
        end
    end
    
    moves = sum(s ~= 0);
   
    % Evaluate
    c = moves + totalBoxDistance + scoreBoxes + deadlockPenalty;
end

s = [1, 2, 0, 3, 4];

data = struct();
    map.map = [
    '######';
    '# $ .#';
    '#@   #';
    '######'
    ];

test = evalFunction(s, data);