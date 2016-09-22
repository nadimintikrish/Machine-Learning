#include <iostream>
#include <vector>
#include <iterator>
#include<sstream>
#include<cstdlib>
#include<string>
#include <fstream>
#include <set>
#include<cmath>
#include <boost/tokenizer.hpp>
#include <boost/foreach.hpp>
#include<omp.h>

typedef std:: vector<std::string> tokenWord;

using namespace std;
using namespace boost;

/**Structure for storing the Word probabilities**/

struct class_t {
vector<float> wordProbability;
string title;
double classProb;
double prodInit=0.0;
double naivBayeProb;
}classes[20];

int  testDataSet(string arrayClass[25],int totWordPos[20],int vocab);
int main()
{

typedef tokenizer< boost::char_separator<char>> Tokenizer;
boost::char_separator<char> sep(" ");
    string data("Trainingset1.data");



   ifstream in(data.c_str());
    if (!in.is_open()) return 1;

    int i=0;
    int j=0;
    int counter=0;
    int k=0;
    int n=0;
    string line;
    tokenWord tokentemp;
    string linebuffer;
    string arrayClass[25];
    int docnum[25];
    int totWordPos[20];

    /** Set for Vocabulary**/

    set<std::string>unique_tokens;

    while(getline(in,line)){

    string tempString;

    Tokenizer tok(line);
    Tokenizer::const_iterator token_iterator= tok.begin();
    Tokenizer tokens{line,sep};

    tempString= *token_iterator;
    tokentemp.push_back(tempString);

/**Vocab Set gets inserted**/
    unique_tokens.insert(tokens.begin(),tokens.end());

/**Counter for total number of Words**/

    BOOST_FOREACH (const string& t, tokens) {

            ++counter;
    }

    if(i==0){

    linebuffer=line;

    }

    else if(i>0&&(tempString==tokentemp.at(i-1))){

    linebuffer+=" ";
    linebuffer+=line;
    k++;

    }

    else if(i>0&&(tempString!=tokentemp.at(i-1))){

        /**Appended String Added to Array of Strings**/

    arrayClass[j]= linebuffer;
    Tokenizer tokensn{linebuffer,sep};

     BOOST_FOREACH (const string& t, tokensn) {

            ++n;
    }
    totWordPos[j]=n;            /** Array for total word count in each class j **/


       /** Num of documents per class**/

        docnum[j]=k+1;

        linebuffer.clear();
        linebuffer=line;
        j++;
        k=0;
        n=0;
    }
    else {

    }

i++;

    classes[j].title=tempString;

    }
    in.close();
    int totDocs=i;
    int vocab = unique_tokens.size();
    //cout<<"Vocab Count"<<vocab<<endl;

     arrayClass[j]= linebuffer;

        Tokenizer tokNew{linebuffer,sep};
      BOOST_FOREACH (const string& t, tokNew) {

            ++n;
    }

    totWordPos[j]=n;

    docnum[j]=k;

    linebuffer.clear();
  //  cout<<"Total Word Count"<<counter<<endl;

    for(int i=0;i<20;i++){

        classes[i].classProb = ((double)(docnum)[i]/(double)(totDocs));

    }
testDataSet(arrayClass,totWordPos,vocab);
}


int  testDataSet(string arrayClass[25],int totWordPos[20],int vocab){

    typedef tokenizer< boost::char_separator<char>> Tokenizer;
    boost::char_separator<char> sep(" ");
    string data("TestSetSmall2.data");
   ifstream in(data.c_str());
    if (!in.is_open()) return -1;

    int linenum=0;
    string line;

    while(getline(in,line)){

    Tokenizer tokens(line,sep); // Tokenizes test data line


     BOOST_FOREACH (const string& temp, tokens) {
        int j=0;

/**Loop for calculating the Word probability in each class**/

        for(int i=0;i<20;i++)
        {
        int wordCount=0;

       Tokenizer tokenNew(arrayClass[i],sep);
 /**Tokenizing the string for each class**/

       // BOOST_FOREACH (const string& tempTrain,tokenNew) {
          for(Tokenizer::iterator tempTrain = tokenNew.begin();tempTrain != tokenNew.end();++tempTrain )
          {

            if(temp== (*tempTrain)){

                ++wordCount;
            }
         }
      //  }
         //  cout<<"Word Count in Line"<<wordCount<<"\t"<<j<<endl;
           classes[i].prodInit = (classes[i].prodInit)+ log10((double)(wordCount+1))-log10((double)(totWordPos[i]+vocab));
            wordCount=0;
        }
        j++;
    }

    /**Claculates naive bayes equation**/

        for(int i=0;i<20;i++){

            classes[i].naivBayeProb = log10(classes[i].classProb)+classes[i].prodInit;
             //   cout<<classes[i].naivBayeProb<<endl;
        }
        float maxClass= classes[0].naivBayeProb;
        int highest=0;

        /**Check the Class with highest probability**/

        for(int i=1;i<20;i++){

           if(classes[i].naivBayeProb>maxClass)
                highest= i;

        }

        cout<<classes[highest].title<<endl; //Printts the Classes with highest Probability

      /**Clears the Value in Classes document**/

        for(int i=0;i<20;i++){

        classes[i].naivBayeProb=0.0;
        classes[i].prodInit=0.0;

        }

    }

}
