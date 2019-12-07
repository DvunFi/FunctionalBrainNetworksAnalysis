inputPath= 'resources\project\Root.type.ProjectData\input\';
outputPath='resources\project\Root.type.ProjectData\output\adjacencyMatrixes\';
inputDataSetName = 'data-starplus-04847-v7.mat';

mat = dir(strcat(inputPath,'*.mat'));

load(inputDataSetName);

filesInfoTabel = {};
filesInfoTabel{1,1} = 'file_name';
filesInfoTabel{1,2} = 'adjacency_measure';
filesInfoTabel{1,3} = 'count_of_1_values';
filesInfoTabel{1,4} = 'input_data_set';
filesInfoTabelRow = 2;

[i,d,m]=transformIDM_selectTrials(info,data,meta,find([info.cond]~=0)); % seletct non-noisey trials
catDataM = cat(1,d{1:length(d)});
    
correletionM = corr(catDataM);

adjacencyMeasure = 0.05;

while adjacencyMeasure < 0.9
    tmpM = correletionM;
    tmpM(tmpM < adjacencyMeasure) = 0;
    tmpM(tmpM >= adjacencyMeasure) = 1;
    fileName = strcat(outputPath,'adjacency_matrix_with_adjacencyMeasure_',strrep(num2str(adjacencyMeasure),'.','_'),'.csv');
    csvwrite(fileName,tmpM);
    disp(size(tmpM(tmpM ==1)));
    disp(fileName);
    
    filesInfoTabel{filesInfoTabelRow,1} = fileName;
    filesInfoTabel{filesInfoTabelRow,2} = num2str(adjacencyMeasure);
    filesInfoTabel{filesInfoTabelRow,3} = num2str(size(tmpM(tmpM ==1)));
    filesInfoTabel{filesInfoTabelRow,4} = inputDataSetName;
    filesInfoTabelRow = filesInfoTabelRow + 1;
    
    adjacencyMeasure = adjacencyMeasure + 0.05;
end

xlswrite(strcat(outputPath,'info_table.xls'),filesInfoTabel);

fclose('all');