feats = table2array(seasfeatures);
labels = feats(:,end);
feats = feats(:,1:end-1);


[loads,scores,lat,tsq,expl,mu]=pca((zscore(feats)));
figure;bar(expl);
xlabel('percentage of the total variance')
ylabel('principal component')
title('variance x pca')
 
%PCA: etichette
pcx=1;
pcy=2;

figure
gscatter(scores(:,pcx),scores(:,pcy),labels)
xlabel(strcat('PC',num2str(pcx), ' : ',num2str(expl(pcx,:)),'%'));
ylabel(strcat('PC',num2str(pcy), ' : ',num2str(expl(pcy,:)),'%'));
title(strcat('PC',num2str(pcx),' - ',' PC',num2str(pcy)));
 
%distribuzione classi

figure
plot3(scores(:,pcx),scores(:,pcy),labels(:,1),'.')
title(strcat('Distribuzione Classi: ',' PC',num2str(pcx),' - ',' PC',num2str(pcy)))

      

 

figure
for i=1:32
    subplot(6,6,i)
    hist(data(:,i));
    title(num2str(i))
end
 

 

%pca: etichette

lsernam=char('length','trend','seasonality','linearity','curvature','spikiness','e_acf1','stability','lumpiness','entropy','hurst','nonlinearity','alpha','beta','hwalpha','hwbeta','hwgamma','y_acf1','diff1_acf1','diff2_acf1','y_acf5','diff1y_acf5','diff2_acf5','seas_acf1','sediff_acf1','sediff_seacf1','sediff_acf5','y_pacf5','diff1y_pacf5','diff2y_pacf5');


%lsernam=char('length','trend','seasonality','linearity','curvature','spikiness','e_acf1','stability','lumpiness','entropy','hurst','nonlinearity','alpha','beta','hwalpha','hwbeta','hwgamma','ur_pp','ur_kpss','y_acf1','diff1_acf1','diff2_acf1','y_acf5','diff1y_acf5','diff2_acf5','seas_acf1','sediff_acf1','sediff_seacf1','sediff_acf5','lmres_acf1','y_pacf5','diff1y_pacf5','diff2y_pacf5','label');
figure;
biplot(loads(:,[pcx pcy]),'scores',scores(:,[pcx pcy]),'varlabels',lsernam(1:30,:));

xlabel(strcat('PC',num2str(pcx), ' : ',num2str(expl(pcx,:)),'%'));
ylabel(strcat('PC',num2str(pcy), ' : ',num2str(expl(pcy,:)),'%'));
title(strcat('PC',num2str(pcx),' - ',' PC',num2str(pcy)));

 

 

%correlazioni features
matco=corrcoef(feats);
figure
imagesc(matco)
colorbar


anova_func(scores, labels)
anova_func(feats,labels)


%tSNE

figure;
yt = tsne(zscore(feats));
gscatter(yt(:,1), yt(:,2), labels)
hold on
[r c]= size(feats);
for i=1:r
    text(yt(i,1),yt(i,3),num2str(i));
    
end
title('tsne');
