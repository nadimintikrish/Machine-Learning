	   #include  <iostream>
       #include  <fstream>
       #include  <cstring>
       #include  <string>

      using   namespace   std;
      string  dnaseq;
      string  newBufstr;

        float   cpgRatio(int   cpgCount,string   window)
        {
        int   aCount=0   ;
        int   tCount=0   ;
        int   gCount=0   ;
        int   cCount=0   ;
        int   total=0;
        int   i=0;
        float   gcCount=0;
        float   obsToexp;

        for   ( i=0;i<=   window.length();i++)

                  {
                           if (window[i]=='A')
                                     aCount=aCount+1;
                           else   if (window[i]=='T')
                                     tCount=tCount+1;
                           else   if(window[i]=='G')
                                     gCount=gCount+1   ;
                           else
                                     cCount=cCount+1   ;
                           }
        total   =  aCount+tCount+gCount+cCount  ;
        gcCount=   (gCount+cCount)*100/total;
          cout<<"total  length  of  the  sequence=  "<<total<<endl;
          cout<<"Sequence=  "<<window<<endl;

                  cout<<"%GC="<<gcCount<<endl;
                  if(gcCount>50){

                  obsToexp  =   ((float)cpgCount/((float)cCount*(float)gCount))*window.length();
                  }
                  return   obsToexp;
        }
        int   cpgCount(string   s2){
        int   cpgCount  =   0;
        for(int   i=0;i<s2.length();i++)
                  {


          char   *namesub   =  (char*)s2.substr(i,2).c_str();
                  if(strcmp(namesub,"CG")==   0)   {

                           cpgCount=cpgCount+1;
                  }
                  }
                  return   cpgCount;
        }

        int   nucFreq(string  s1)
        {

        int   count1=0   ;
        int   count2=0   ;
        int   count3=0   ;
        int   count4=0   ;
        int   total=0;
        float   gcCount=0;

        for   ( int   i=0;i<=   s1.length();i++)

                  {
                           if (s1[i]=='A')
                                     count1=count1+1;
                           else   if (s1[i]=='T')
                                     count2=count2+1;
                           else   if(s1[i]=='G')
                                     count3=count3+1   ;
                           else
                                     count4=count4+1   ;
                           }
        total   =  count1+count2+count3+count4  ;
        gcCount=   (count3+count4)*100/total;
          cout<<"total  length  of  the  sequence=  "<<total<<endl;
          cout<<"Sequence=  "<<s1<<endl;

                  cout<<"%GC="<<gcCount<<endl;
                  return   gcCount;
        }

        int   main   ()  {
             string   line;
        char   enterstring[50];
        int   enternum;
        cout   <<  "Please  enter  your  Choice  ";
        cout   <<  "GcCount  and percentage:  "<<1<<endl;
        cout<<"CpGisland>75%age:"<<2<<endl;
        cin   >>  enternum;

     ofstream  outfile;
     outfile.open("CpGislandOut.txt");
        ifstream  myfile   ("DnaFiles/Human_chromosome-1_PARK7.fasta");
         string   newSt   ;
         string   bufstring;
         if (myfile.is_open())
         {
             while   ( getline   (myfile,line)   )
             {
               char   nucleotide;
               bufstring   =  newSt.append(line)   ;


             }

             outfile<<"CpGRatio"<<"  "<<"StartIndex"<<"  "<<"LastIndex"<<endl;
             for   (int   i=0;i<bufstring.length();i++)
             {

               newBufstr=   bufstring.substr(i,200);
               if(newBufstr.length()>=200)
               {
                       if(enternum  ==  1)
                      {

                     nucFreq(newBufstr);
                      }
                       else   if(enternum  ==2)
                      {
                   int   cgcount   =   cpgCount(newBufstr);
                     float   cpgR   =cpgRatio(cgcount,newBufstr);
                     if(cpgR   >   0.75){
                   outfile<<cpgR<<"\t"<<   i<<"\t"<<i+200<<endl;
                    }
                    }
                     else
                    {
                              cout<<"enter  the  value  from  the  list";
                    }
               }
             }


             myfile.close();
         }

         else   cout   <<  "Unable  to  open file";

         return   0;
     }

