#include <iostream>
#include <vector>
#include <iterator>
#include<sstream>
#include<cstdlib>
#include<string>
#include <fstream>
#include <boost/tokenizer.hpp>
#include<boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics.hpp>
#include <boost/bind.hpp>
#include <boost/ref.hpp>

typedef std::vector<double> Hours;
typedef std::vector<double> Downloads;
using namespace boost::accumulators;
typedef accumulator_set<double, stats<tag::mean, tag::variance>> accSet;
double slopeEstimate(Hours hrs,Downloads dwnlds,double hrnew);


int main()
{
   using namespace std;
   using namespace boost;

   string data("docML1.csv");

   ifstream in(data.c_str());
    if (!in.is_open()) return 1;

    typedef tokenizer< escaped_list_separator<char> > Tokenizer;

    Hours hour;
    Downloads dwnldnum;
    string line;

    while(getline(in,line)){

        Tokenizer tok(line);
        Tokenizer::const_iterator token_iterator= tok.begin();

        std::istringstream to_double;
        double datum = 0 ;
        double hrnum=0;


        to_double.clear();
		to_double.str(*token_iterator);
		to_double >> hrnum;

        hour.push_back(hrnum);




    for(++token_iterator; token_iterator!=tok.end();++token_iterator){




		to_double.clear();
		to_double.str(*token_iterator);
		to_double >> datum;

       dwnldnum.push_back(datum);


        }



    }

double hrnew= 852; // No of hours computed at noon of fifth day of next month

cout<<"No of Downloads Estimated :"<<slopeEstimate(hour,dwnldnum,hrnew)<<endl;

}

double slopeEstimate(Hours hrs, Downloads dwnld,double hrnew){

accSet accX;
accSet accY;

std::for_each(hrs.begin(), hrs.end() ,
								boost::bind<void>(boost::ref(accX), _1));


std::for_each(dwnld.begin(), dwnld.end() ,
								boost::bind<void>(boost::ref(accY), _1));


double meanX = mean(accX);
double meanY = mean(accY);
double slope=0.0;
double Sxy=0;
double Sxx=0;

for(int i=0; i<hrs.size(); i++){
Sxy= Sxy+ ((hrs.at(i)-meanX)*(dwnld.at(i)-meanY));
Sxx= Sxx+ ((hrs.at(i)-meanX)*(hrs.at(i)-meanX));

}
std::cout<<"Sxx :"<<Sxx<<std::endl;
std::cout<<"Sxy :"<<Sxy<<std::endl;

slope= (Sxy/Sxx);

std::cout<<"Slope :" << slope<< std::endl;

double intercept= (meanY-slope*meanX);

std::cout<<"Intercept :" << intercept<< std::endl;

double response = slope*hrnew+intercept;

return response;
}

