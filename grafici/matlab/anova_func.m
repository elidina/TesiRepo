function p=anova_func(df,class,nome)
 

data=df;
type='Features';
main_title ='ANOVA: Features';
 

[r c]=size(data);
 

numclas=max(class);
a=sqrt(c);
 for i=1:c
    p(i)=anova1(data(:,i),class,'off');
%     close gcf
%     close gcf
 end
 

 figure
 if nargin<3
     for i=1:c
         subplot(subplot(round(a),ceil(a),i));
         boxplot(data(:,i),class)
         title (strcat(type,' :',num2str(i)))
         %ylabel('frequency shift [Hz]')
     end
 end
 

 if nargin==3
     for i=1:c
         subplot(subplot(round(a),ceil(a),i));
         boxplot(data(:,i),class)
         title (strcat(type,' :',num2str(i)))
         set(gca,'xticklabel',nome)
     end
 end
 
 suptitle(main_title)
 