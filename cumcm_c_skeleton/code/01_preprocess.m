%% Reproducible preprocessing for the ancient-glass dataset (MATLAB R2025b).
% This source is ASCII-only because the local MATLAB launcher rejects non-ASCII
% script source. Unicode workbook labels are generated with char code points.
clear; clc;

rootDir = fileparts(fileparts(mfilename('fullpath')));
projectDir = fileparts(rootDir);
qdir = fullfile(projectDir, 'question');
source = dir(fullfile(qdir, '*.xlsx'));
assert(numel(source) == 1, 'Expected exactly one input workbook in question/.');
inputFile = fullfile(source.folder, source.name);
dataDir = fullfile(rootDir, 'data'); snapDir = fullfile(dataDir, 'verification_snapshots'); figDir = fullfile(rootDir, 'figures');
if ~isfolder(dataDir), mkdir(dataDir); end
if ~isfolder(snapDir), mkdir(snapDir); end
if ~isfolder(figDir), mkdir(figDir); end
delta = 0.02; deltas = [0.01 0.02 0.04];

T1 = readtable(inputFile, Sheet=1, TextType='string', VariableNamingRule='preserve');
T2 = readtable(inputFile, Sheet=2, TextType='string', VariableNamingRule='preserve');
T3 = readtable(inputFile, Sheet=3, TextType='string', VariableNamingRule='preserve');
assert(height(T1)==58 && height(T2)==69 && height(T3)==8, 'Unexpected workbook dimensions.');
aid=string(T1{:,1}); decor=string(T1{:,2}); typ=string(T1{:,3}); color=string(T1{:,4}); aw=string(T1{:,5});
sname=string(T2{:,1}); raw=T2{:,2:15}; raw(isnan(raw))=0;
uid=string(T3{:,1}); uw=string(T3{:,2}); uraw=T3{:,3:16}; uraw(isnan(uraw))=0;
sid=extractBetween(sname,1,2);
ss=sum(raw,2); valid=ss>=85 & ss<=105; us=sum(uraw,2);
assert(sum(valid)==67 && all(us>=85 & us<=105), 'Validity-screening assertion failed.');
assert(all(ismember(["15";"17"],sid(~valid))), 'Invalid points must be 15 and 17.');
nonDetect=sum(raw(valid,:)==0,'all'); pos=raw(valid,:); pos=pos(pos>0); assert(abs(min(pos)-0.04)<1e-12, 'Minimum positive value assertion failed.');

vr=raw(valid,:); closed=closure(vr); replaced=mreplace(closed,delta); Z=clr(replaced);
mu=mean(Z,1); sig=std(Z,0,1); zZ=(Z-mu)./sig;
uclosed=closure(uraw); ureplaced=mreplace(uclosed,delta); uZ=clr(ureplaced); uzZ=(uZ-mu)./sig;
assert(all(abs(sum(closed,2)-100)<1e-10) && all(abs(sum(replaced,2)-100)<1e-10), 'Closure assertion failed.');

% Artifact representative composition is used only for color imputation.
rep=nan(height(T1),14);
severeIds=["08","26","54"]; unweatheredIds=["23","25","28","29","42","44","49","50","53"];
for i=1:height(T1)
    ix=valid & sid==aid(i);
    if any(ix)
        nms=sname(ix); x=raw(ix,:); ids=sid(ix);
        severe=ismember(ids,severeIds); unweathered=ismember(ids,unweatheredIds); ordinary=~severe & ~unweathered;
        if any(ordinary) && any(severe), rep(i,:)=mean(x(ordinary,:),1)/3+2*mean(x(severe,:),1)/3;
        elseif any(ordinary), rep(i,:)=mean(x(ordinary,:),1);
        elseif any(unweathered), rep(i,:)=mean(x(unweathered,:),1);
        else, rep(i,:)=mean(x,1); end
    end
end
rZ=clr(mreplace(closure(rep),delta));
miss=ismissing(color) | strlength(strtrim(color))==0; assert(sum(miss)==4, 'Expected four missing colors.');
filled=color; method=repmat("",height(T1),1); donors=repmat("",height(T1),1); distances=repmat("",height(T1),1); share=nan(height(T1),1);
for i=find(miss)'
    mask=~miss & typ==typ(i) & decor==decor(i) & all(isfinite(rZ),2); ix=find(mask);
    d=sqrt(sum((rZ(ix,:)-rZ(i,:)).^2,2)); [d,o]=sort(d); ix=ix(o); take=min(5,numel(ix)); ix=ix(1:take); d=d(1:take); w=1./max(d,eps);
    labs=color(ix); u=unique(labs,'sorted'); score=zeros(numel(u),1); for j=1:numel(u), score(j)=sum(w(labs==u(j))); end
    [best,j]=max(score); share(i)=best/sum(score);
    if share(i)>=.5, filled(i)=u(j); method(i)="hot-deck";
    else
        candidates=color(~miss & typ==typ(i)); uu=unique(candidates,'sorted'); count=zeros(numel(uu),1); for j=1:numel(uu), count(j)=sum(candidates==uu(j)); end
        [~,j]=max(count); filled(i)=uu(j); method(i)="type-mode-fallback";
    end
    donors(i)=strjoin(aid(ix),','); distances(i)=strjoin(compose('%.8f',d),',');
end

% CLR-space robust diagnostics by glass type and binary point weathering.
n=sum(valid); vtype=strings(n,1); bw=strings(n,1); pointTag=strings(n,1); vsid=sid(valid); vsname=sname(valid);
for q=1:n
    a=find(aid==vsid(q),1); vtype(q)=typ(a);
    if ismember(vsid(q),unweatheredIds), bw(q)="unweathered"; pointTag(q)="unweathered-point";
    else
        bw(q)=ternary(strlength(aw(a))==3,"unweathered","weathered");
        pointTag(q)=ternary(ismember(vsid(q),severeIds),"severe-weathered-point","ordinary-point");
    end
end
[D,madT,iqrT,outlier]=diagnose(Z,vtype,bw); assert(~any(outlier), 'Unexpected additional robust outlier.');
newOut=zeros(numel(deltas),1);
for h=1:numel(deltas), [~,~,~,o]=diagnose(clr(mreplace(closed,deltas(h))),vtype,bw); newOut(h)=sum(o); end
assert(all(newOut==0), 'Sensitivity assertion failed.');

% Three raw snapshots and a key numerical snapshot for the R verification.
comp="component_"+string(1:14);
S1=table(aid,decor,typ,color,aw,VariableNames={'artifact_id','decoration','glass_type','raw_color','artifact_weather'});
S2=array2table(raw,VariableNames=comp); S2=addvars(S2,sname,sid,ss,valid,Before=1,NewVariableNames={'sample_name','artifact_id','composition_sum','valid'});
S3=array2table(uraw,VariableNames=comp); S3=addvars(S3,uid,uw,us,Before=1,NewVariableNames={'artifact_id','weather','composition_sum'});
writetable(S1,fullfile(snapDir,'sheet1_raw_snapshot.csv'),Encoding='UTF-8'); writetable(S2,fullfile(snapDir,'sheet2_raw_snapshot.csv'),Encoding='UTF-8'); writetable(S3,fullfile(snapDir,'sheet3_raw_snapshot.csv'),Encoding='UTF-8');
K=array2table(Z,VariableNames="CLR_"+string(1:14)); K=addvars(K,vsname,vsid,ss(valid),D,outlier,Before=1,NewVariableNames={'sample_name','artifact_id','raw_sum','aitchison_distance','robust_outlier'}); writetable(K,fullfile(snapDir,'matlab_key_results.csv'),Encoding='UTF-8');

% Workbook sheets match the delivery inventory; columns use stable ASCII names.
outFile=fullfile(dataDir,cn('attachment')); if isfile(outFile), delete(outFile); end
writecell({'Preprocessing notes';'MATLAB main calculation; R independent verification.';'Blanks are non-detects and become zero except before CLR.'},outFile,Sheet=cn('notes'));
AC=table(aid,decor,typ,color,filled,aw,method,donors,distances,share,VariableNames={'artifact_id','decoration','glass_type','raw_color','final_color','artifact_weather','fill_method','donor_ids','donor_distances','winner_share'}); writetable(AC,outFile,Sheet=cn('artifact_clean'));
SC=table(sname,sid,ss,valid,VariableNames={'sample_name','artifact_id','composition_sum','valid'}); writetable(SC,outFile,Sheet=cn('sample_clean'));
VO=array2table(vr,VariableNames=comp); VO=addvars(VO,vsname,vsid,ss(valid),Before=1,NewVariableNames={'sample_name','artifact_id','raw_sum'}); writetable(VO,outFile,Sheet=cn('valid_raw'));
writeStage(outFile,cn('closed'),vsname,vsid,closed,comp); writeStage(outFile,cn('replace'),vsname,vsid,replaced,comp); writeStage(outFile,cn('clr'),vsname,vsid,Z,comp); writeStage(outFile,cn('zclr'),vsname,vsid,zZ,comp);
DG=table(vsname,vsid,vtype,pointTag,bw,D,madT,iqrT,outlier,VariableNames={'sample_name','artifact_id','glass_type','point_weather_tag','binary_weather','aitchison_distance','mad_threshold','iqr_threshold','robust_outlier'}); writetable(DG,outFile,Sheet=cn('diagnostic'));
PT=array2table([mu;sig],VariableNames=comp); PT=addvars(PT,["mean";"std"],Before=1,NewVariableNames='statistic'); writetable(PT,outFile,Sheet=cn('parameters'));
UO=array2table(uraw,VariableNames=comp); UO=addvars(UO,uid,uw,us,Before=1,NewVariableNames={'artifact_id','weather','raw_sum'}); writetable(UO,outFile,Sheet=cn('unknown_raw'));
writeStage(outFile,cn('unknown_closed'),uid,strings(height(T3),1),uclosed,comp); writeStage(outFile,cn('unknown_replace'),uid,strings(height(T3),1),ureplaced,comp); writeStage(outFile,cn('unknown_clr'),uid,strings(height(T3),1),uZ,comp); writeStage(outFile,cn('unknown_zclr'),uid,strings(height(T3),1),uzZ,comp);
writetable(table(deltas',newOut,VariableNames={'delta_percent','new_robust_outliers'}),outFile,Sheet=cn('sensitivity'));

makeFigures(ss,valid,vr,figDir);
fid=fopen(fullfile(dataDir,'matlab_preprocess_summary.txt'),'w');
fprintf(fid,'MATLAB preprocessing: PASS\nArtifacts=58; raw points=69; valid points=67; unknown=8.\n');
fprintf(fid,'Invalid chemistry: 15=79.47, 17=71.89. Undetected valid cells=%d (%.2f%%).\n',nonDetect,100*nonDetect/numel(vr));
fprintf(fid,'Color outputs: %s.\n',strjoin(aid(miss)+"="+filled(miss)+"("+method(miss)+")",'; ')); fprintf(fid,'Additional robust outliers=0.\n'); fclose(fid);
fprintf('Completed: %s\n',outFile);

function C=closure(X), C=100.*X./sum(X,2); end
function Y=mreplace(X,d)
Y=X; for r=1:size(X,1), z=X(r,:)==0; k=sum(z); if k>0, Y(r,z)=d; Y(r,~z)=X(r,~z).*(100-k*d)/100; end, end
end
function Z=clr(X), Z=log(X./geomean(X,2)); end
function x=ternary(c,a,b), if c,x=a;else,x=b;end,end
function [D,mt,it,mark]=diagnose(Z,t,w)
n=size(Z,1);D=nan(n,1);mt=nan(n,1);it=nan(n,1);mark=false(n,1);g=unique(t+"|"+w);
for h=g', ix=(t+"|"+w)==h; c=median(Z(ix,:),1); D(ix)=sqrt(sum((Z(ix,:)-c).^2,2)); d=D(ix); m=median(d); mt(ix)=m+3.5*median(abs(d-m)); q=prctile(d,[25 75]); it(ix)=q(2)+1.5*(q(2)-q(1)); mark(ix)=d>mt(ix)&d>it(ix); end
end
function writeStage(file,sheet,ids,arts,X,names)
T=array2table(X,VariableNames=names); if all(strlength(arts)==0), T=addvars(T,ids,Before=1,NewVariableNames='artifact_id'); else,T=addvars(T,ids,arts,Before=1,NewVariableNames={'sample_name','artifact_id'});end; writetable(T,file,Sheet=sheet);
end
function s=cn(k)
switch k
case 'attachment', s=char([38468 20214 95 25968 25454 39044 22788 29702 32467 26524 46 120 108 115 120]);
case 'notes', s=char([22788 29702 35828 26126]); case 'artifact_clean',s=char([34920 21333 49 28165 27927 21450 39068 33394 22635 34917]);
case 'sample_clean',s=char([34920 21333 50 28165 27927 35760 24405]); case 'valid_raw',s=char([26377 25928 21407 22987 25968 25454]);
case 'closed',s=char([38381 21512 24402 19968 21270 25968 25454]); case 'replace',s=char([36817 38646 26367 25442 25968 25454]);
case 'clr',s=char([67 76 82 21407 22987 20540]); case 'zclr',s=char([90 45 67 76 82 25968 25454]);
case 'diagnostic',s=char([24322 24120 35786 26029]); case 'parameters',s=char([67 76 82 26631 20934 21270 21442 25968]);
case 'unknown_raw',s=char([34920 21333 51 23545 24212 22788 29702 32467 26524]); case 'unknown_closed',s='U3_closed'; case 'unknown_replace',s='U3_replace'; case 'unknown_clr',s='U3_CLR'; case 'unknown_zclr',s='U3_ZCLR'; case 'sensitivity',s=char([36817 38646 21442 25968 25935 24863 24615]);
end
end
function makeFigures(sums,valid,X,figDir)
f=figure(Visible='off',Color='w',Position=[100 100 1200 430]);t=tiledlayout(f,1,2,TileSpacing='compact',Padding='compact');
ax=nexttile(t);set(ax,Color='w',XColor='k',YColor='k',GridColor=[.7 .7 .7]);histogram(sums(valid),12,FaceColor=[.12 .42 .68]);hold on;histogram(sums(~valid),FaceColor=[.75 .3 .12]);xline(85,'--k');xline(105,'--k');title('Composition-sum validity',Color='k');xlabel('sum (%)',Color='k');ylabel('points',Color='k');legend('valid','invalid',TextColor='k',Color='w',Location='best');grid on;
ax=nexttile(t);set(ax,Color='w',XColor='k',YColor='k',GridColor=[.7 .7 .7]);bar(100*sum(X==0,1)/size(X,1),FaceColor=[.12 .42 .68]);title('Non-detect rate by component',Color='k');xlabel('component',Color='k');ylabel('rate (%)',Color='k');grid on;exportgraphics(f,fullfile(figDir,'data_preprocess_diagnostics.pdf'),ContentType='image',Resolution=300,BackgroundColor='white');close(f);
f=figure(Visible='off',Color='w',Position=[100 100 1200 300]);ax=axes(f,Color='w',XColor='k',YColor='k');axis(ax,[0 12 0 2]);axis(ax,'off');labels={'Read sheets','Validity screen','Closure','Zero replacement','CLR / Z-CLR','Outputs'};for q=1:6,x=.2+(q-1)*2;rectangle(ax,Position=[x .7 1.45 .55],Curvature=.08,FaceColor=[.9 .96 .93],EdgeColor=[.1 .35 .25],LineWidth=1);text(ax,x+.72,.97,labels{q},HorizontalAlignment='center',Color=[.05 .12 .08],FontWeight='bold');if q<6,text(ax,x+1.6,.97,'->',HorizontalAlignment='center',Color='k',FontWeight='bold');end,end;exportgraphics(f,fullfile(figDir,'data_preprocess_flow.pdf'),ContentType='image',Resolution=300,BackgroundColor='white');close(f);
end
