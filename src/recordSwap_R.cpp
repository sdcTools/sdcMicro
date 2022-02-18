#include <Rcpp.h>
using namespace Rcpp;

// Enable C++11 via this plugin (Rcpp 0.10.3 or later) - used for R only
// [[Rcpp::plugins(cpp11)]] 

#include "recordSwap/recordSwap.cpp"


//' @title Targeted Record Swapping
//' 
//' @description Applies targeted record swapping on micro data set, see \code{?recordSwap} for details.
//' \cr
//' \strong{NOTE:} This is an internal function called by the R-function \code{recordSwap()}. It's only purpose is to include the C++-function recordSwap() using Rcpp.
//' 
//' @param data micro data set containing only integer values. A data.frame or data.table from R needs to be transposed beforehand so that data.size() ~ number of records - data.[0].size ~ number of varaibles per record.
//' \strong{NOTE:} \emph{data has to be ordered by hid beforehand.}
//' @param similar_cpp List where each entry corresponds to column indices of variables in \code{data} which should be considered when swapping households.
//' @param hierarchy column indices of variables in \code{data} which refere to the geographic hierarchy in the micro data set. For instance county > municipality > district.
//' @param risk_variables column indices of variables in \code{data} which will be considered for estimating the risk.
//' @param hid column index in \code{data} which refers to the household identifier.
//' @param k_anonymity integer defining the threshhold of high risk households (k-anonymity). This is used as k_anonymity <= counts.
//' @param swaprate double between 0 and 1 defining the proportion of households which should be swapped, see details for more explanations
//' @param risk_threshold double indicating risk threshold above every household needs to be swapped.
//' @param risk vector of vectors containing risks of each individual in each hierarchy level.
//' @param carry_along integer vector indicating additional variables to swap besides to hierarchy variables.
//' These variables do not interfere with the procedure of finding a record to swap with or calculating risk.
//' This parameter is only used at the end of the procedure when swapping the hierarchies.
//' @param log_file_name character, path for writing a log file. The log file contains a list of household IDs (`hid`) which could not have been swapped and is only created if any such households exist.    
//' @param seed integer defining the seed for the random number generator, for reproducability.
//' 
//' @return Returns data set with swapped records.
// [[Rcpp::export]]
std::vector< std::vector<int> > recordSwap_cpp(std::vector< std::vector<int> > data, int hid,
                                               std::vector<int> hierarchy, Rcpp::List similar_cpp,
                                               double swaprate, 
                                               std::vector<std::vector<double>> risk, double risk_threshold,
                                               int k_anonymity, std::vector<int> risk_variables,
                                               std::vector<int> carry_along,
                                               std::string log_file_name,
                                               int seed = 123456){
  // prep inputs for the call to recordSwap()
  // some formats can not directly be transformed to stl-containers
  std::vector<std::vector<int>> similar(similar_cpp.size());
  for(int i=0;i<similar_cpp.size();i++){
    Rcpp::List sublist = similar_cpp[i];
    int m = sublist.size();
    for(int j=0;j<m;j++){
      similar[i].push_back(sublist[j]);
    }
  }
  
  int count_swapped_hid = 0;
  int count_swapped_records = 0;
  
  // call recrodSwap()
  std::vector< std::vector<int> > output = recordSwap(data, hid,
                                                      hierarchy, similar,
                                                      swaprate,
                                                      risk, risk_threshold,
                                                      k_anonymity, risk_variables,  
                                                      carry_along,
                                                      count_swapped_records = count_swapped_records,
                                                      count_swapped_hid = count_swapped_hid,
                                                      log_file_name = log_file_name,
                                                      seed);
  
  return output;
}

//' @title Define Swap-Levels
//' 
//' @description Define hierarchy levels over which record needs to be swapped according to risk variables.
//' \cr
//' \strong{NOTE:} This is an internal function used for testing the C++-function \code{setLevels()} which is applied inside \code{recordSwap()}.
//' 
//' 
//' @param risk vector of vectors containing risks of each individual in each hierarchy level. \code{risk[0]} returns the vector of risks for the first unit over all hierarchy levels.
//' \code{risk[1]} the vector if risks for all hierarchy level of unit 2, and so on.
//' @param risk_threshold double defining the risk threshold beyond which a record/household needs to be swapped. This is understood as risk>=risk_threshhold.
//' 
//' @return Integer vector with hierarchy level over which record needs to be swapped with.
// [[Rcpp::export]]
std::vector<int> setLevels_cpp(std::vector< std::vector<double> > risk, double risk_threshold) {
  std::vector<int> output = setLevels(risk, risk_threshold);
  return output;
}

//' @title Reorder data
//' 
//' @description Reorders the data according to a column in the data set.
//' \cr
//' \strong{NOTE:} This is an internal function used for testing the C++-function \code{orderData} which is used inside the C++-function \code{recordSwap()} to speed up performance.
//' 
//' @param data micro data set containing only numeric values.
//' @param orderIndex column index in \code{data} refering to the column by which data should be ordered.
//' 
//' @return ordered data set.
// [[Rcpp::export]]
std::vector< std::vector<int> > orderData_cpp(std::vector< std::vector<int> > &data, int orderIndex){
  std::vector< std::vector<int> > output = orderData(data,orderIndex) ;
 return output ;
}


//' @title Calculate Risk
//' 
//' @description Calculate risk for records to be swapped and donor records.  Risks are defined by 1/counts, where counts is the number of records with the same values for specified \code{risk_variables} in the each geographic hierarchy.
//' This risk will be used as sampling probability for both sampling set and donor set.
//' \cr
//' \strong{NOTE:} This is an internal function used for testing the C++-function \code{setRisk} which is used inside the C++-function \code{recordSwap()}.
//' 
//' @param data micro data set containing only numeric values.
//' @param hierarchy column indices of variables in \code{data} which refere to the geographic hierarchy in the micro data set. For instance county > municipality > district.
//' @param risk_variables column indices of variables in \code{data} which will be considered for estimating the risk.
//' @param hid column index in \code{data} which refers to the household identifier.
//' 
// [[Rcpp::export]]
std::vector< std::vector<double> > setRisk_cpp(std::vector<std::vector<int> > data, std::vector<int> hierarchy, std::vector<int> risk_variables, int hid){
  std::vector< std::vector<double> > output = setRisk(data,hierarchy,risk_variables,hid);
  return output;
}


//' @title Random Sampling
//' 
//' @description Randomly select records given a proabability weight vector \code{prob}. 
//' \cr
//' \strong{NOTE:} This is an internal function used for testing the C++-function \code{randSample} which is used inside the C++-function \code{recordSwap()}.
//' 
//' @param ID vector containing record IDs from which to sample
//' @param N integer defining the number of records to be sampled
//' @param prob a vector of probability weights for obtaining the elements of the vector being sampled.
//' @param IDused vector containing IDs which must not be sampled
//' @param seed integer setting the sampling seed
//' 
// [[Rcpp::export]]
std::vector<int> randSample_cpp(std::vector<int> ID, int N, std::vector<double> prob,std::vector<int> IDused, int seed){
  
  // set random seed according to input parameter and
  // initialize random number generator with seed
  std::mt19937 mersenne_engine;
  mersenne_engine.seed(seed);
  
  std::unordered_set<int> mustSwap;
  std::unordered_set<int> ID_set(ID.begin(),ID.end());
  
  std::vector<int> output = randSample(ID_set,N,prob,mersenne_engine,IDused,mustSwap);
  
  return output;
}


//' @title Distribute number of swaps
//' 
//' @description Distribute number of swaps across lowest hierarchy level according to a predefinde \code{swaprate}. The swaprate is applied such that a single swap counts as swapping 2 households.
//' Number of swaps are randomly rouded up or down, if needed, such that the total number of swaps is in coherence with the swaprate.
//' \cr
//' \strong{NOTE:} This is an internal function used for testing the C++-function \code{distributeDraws} which is used inside the C++-function \code{recordSwap()}.
//' 
//' @param data micro data containing the hierarchy levels and household ID
//' @param hierarchy column indices of variables in \code{data} which refere to the geographic hierarchy in the micro data set. For instance county > municipality > district.
//' @param hid column index in \code{data} which refers to the household identifier.
//' @param swaprate double between 0 and 1 defining the proportion of households which should be swapped, see details for more explanations
//' @param seed integer setting the sampling seed
//' 
// [[Rcpp::export]]
std::vector< std::vector<int> > distributeDraws_cpp(std::vector< std::vector<int> > data,
                                                     std::vector<int> hierarchy, int hid, double swaprate, int seed = 123456){
  
  // define parameter
  int n = data.size();
  int nhier = hierarchy.size();
  int nhid = 0;
  int currentID = -1;
  
  std::mt19937 mersenne_engine;
  mersenne_engine.seed(seed);
  // initialize random number generator for distributeDraws()
  std::uniform_int_distribution<std::mt19937::result_type> runif01(0,1);
  
  std::map<std::vector<int>,std::unordered_set<int> > group_hier; //
  std::vector<int> hier_help(nhier); // help vector to get hierarchy groups
  
  for(int i=0;i<n;i++){
    
    if(currentID==data[i][hid]){
      continue; // go to next iteration if statement is true
    }
    
    currentID = data[i][hid];
    // ... define hierarchy group
    for(int j=0;j<nhier;j++){
      hier_help[j] = data[i][hierarchy[j]];
    }
    
    // supply new household index to each group
    // use only indices to speed up construction of output data
    group_hier[hier_help].insert(i);
    
    // count number of households
    nhid++;
    // skip all other household member, only need first one
  }
  
  std::map<std::vector<int>,std::pair<int,int>> draw_group =  distributeDraws(group_hier, nhid, swaprate, 
                                                                              runif01, mersenne_engine);
  
  
  // iterate over map to generate output
  // implementation good enough for testing purposes
  std::vector<std::vector<int>> output(draw_group.size(),std::vector<int>(nhier+2));
  int z = 0;
  for(auto const&x : draw_group){
    // std::cout<<"hierarchy:" << " ";
    for(int j=0;j<(nhier+2);j++){
      if(j<nhier){
        // std::cout<<x.first[j] << " ";
        output[z][j] = x.first[j];
      }else if(j==nhier){
        // std::cout<<"\n n1: "<<x.second.first;
        output[z][j] = x.second.first;
      }else{
        // std::cout<<"\n n2: "<<x.second.second<<"\n";
        output[z][j] = x.second.second;
      }
    }
    z++;
  }
  return output;
}

// [[Rcpp::export]]
std::vector<std::vector<int>> distributeDraws2_cpp(std::vector< std::vector<int> > data, std::vector< std::vector<double> > risk,
                                                    std::vector<int> hierarchy, int hid, double swaprate, int seed = 123456){
  
  // define parameter
  int n = data.size();
  int nhier = hierarchy.size();
  int nhid = 0;
  int currentID = -1;
  
  std::mt19937 mersenne_engine;
  mersenne_engine.seed(seed);
  // initialize random number generator for distributeDraws()
  std::uniform_int_distribution<std::mt19937::result_type> runif01(0,1);
  
  std::map<std::vector<int>,std::unordered_set<int> > group_hier; //
  std::vector<int> hier_help(nhier); // help vector to get hierarchy groups
  
  for(int i=0;i<n;i++){
    
    if(currentID==data[i][hid]){
      continue; // go to next iteration if statement is true
    }
    
    currentID = data[i][hid];
    // ... define hierarchy group
    for(int j=0;j<nhier;j++){
      hier_help[j] = data[i][hierarchy[j]];
    }
    
    // supply new household index to each group
    // use only indices to speed up construction of output data
    group_hier[hier_help].insert(i);
    
    // count number of households
    nhid++;
    // skip all other household member, only need first one
  }
  // std::cout << "number of households: " << nhid << "\n";
  std::map<std::vector<int>,int> draw_group =  distributeDraws2(group_hier, risk, nhid, swaprate, runif01, mersenne_engine);
  
  
  // iterate over map to generate output
  // implementation good enough for testing purposes
  std::vector<std::vector<int>> output(draw_group.size(),std::vector<int>(nhier+1));
  int z = 0;
  for(auto const&x : draw_group){
    //std::cout<<"hierarchy:" << " ";
    int xn = x.first.size();
    for(int j=0;j<(nhier+1);j++){
      if((j<nhier) & (j<xn)){
        //std::cout<<x.first[j] << " ";
        output[z][j] = x.first[j];
      }else if(j==nhier){
        //std::cout<<"\n n1: "<<x.second;
        output[z][j] = x.second;
      }
    }
    //std::cout<<"\n";
    z++;
  }
  
  return output;
}


/*
 * Function to test sampleDonor
 */
//' @title Random sample for donor records
//' 
//' @description Randomly select donor records given a probability weight vector. This sampling procedure is implemented differently than \code{\link{randSample_cpp}} to speed up performance of C++-function \code{recordSwap()}.
//' \cr
//' \strong{NOTE:} This is an internal function used for testing the C++-function \code{sampleDonor} which is used inside the C++-function \code{recordSwap()}.
//' 
//' @param data micro data containing the hierarchy levels and household ID
//' @param similar_cpp List where each entry corresponds to column indices of variables in \code{data} which should be considered when swapping households.
//' @param hid column index in \code{data} which refers to the household identifier.
//' @param IDswap vector containing records for which a donor needs to be sampled
//' @param IDswap_pool_vec set from which `IDswap` was drawn
//' @param prob a vector of probability weights for obtaining the elements of the vector being sampled.
//' @param seed integer setting the sampling seed
//' 
// [[Rcpp::export]]
std::vector<int> sampleDonor_cpp(std::vector< std::vector<int> > data, Rcpp::List similar_cpp, int hid,
                                  std::vector<int> IDswap, std::vector<int> IDswap_pool_vec, std::vector<double> prob, int seed=123456){
  
  
  // prep inputs for the call to sampleDonor()
  // some formats can not directly be transformed to stl-containers
  std::vector<std::vector<int>> similar(similar_cpp.size());
  for(int i=0;i<similar_cpp.size();i++){
    Rcpp::List sublist = similar_cpp[i];
    int m = sublist.size();
    for(int j=0;j<m;j++){
      similar[i].push_back(sublist[j]);
    }
  }
  
  // generate paramters
  int n = data.size();
  std::vector<int> IDused(n);
  std::unordered_set<int> IDswap_pool(IDswap_pool_vec.begin(),IDswap_pool_vec.end());
  
  // generate IDdonor_pool
  std::mt19937 mersenne_engine;
  mersenne_engine.seed(seed);
  // initialise lambda para for exp distribution
  std::exponential_distribution<double> exp_dist(1.0);
  std::map<double,int> IDdonor_pool;
  for(int i=0; i<n; i++){
    IDdonor_pool[prob[i]/exp_dist(mersenne_engine)] = i;
  }
  
  std::vector<int> IDdonor = sampleDonor(data, similar, IDswap, IDswap_pool,
                                         IDdonor_pool, IDused, hid);
  
  return IDdonor;
}


/*
 * test randomly distributing stuff
 */
//' @title Distribute 
//' 
//' @description Distribute `totalDraws` using ratio/probability vector `inputRatio` and randomly round each entry up or down such that the distribution results in an integer vector.
//' Returns an integer vector containing the number of units in `totalDraws` distributetd according to proportions in `inputRatio`.
//' \cr
//' \strong{NOTE:} This is an internal function used for testing the C++-function \code{distributeRandom} which is used inside the C++-function \code{recordSwap()}.
//' 
//' @param inputRatio vector containing ratios which are used to distribute number units in `totalDraws`.
//' @param totalDraws number of units to distribute
//' @param seed integer setting the sampling seed
//' 
// [[Rcpp::export]]
std::vector<int> distributeRandom_cpp(std::vector<double> inputRatio, int totalDraws,
                                      int seed){
  
  // prep input
  std::mt19937 mersenne_engine;
  mersenne_engine.seed(seed);
  
  std::vector<int> help_i(1,0);
  std::map<std::vector<int>,double> ratioDraws;
  for(std::size_t i;i<inputRatio.size();i++){
    help_i[0] = i;
    ratioDraws[help_i] = inputRatio[i];
  }
  
  // output
  std::map<std::vector<int>,int> numberDraws;
  
  numberDraws = distributeRandom(ratioDraws, totalDraws,mersenne_engine);
  
  // return output
  std::vector<int> output(numberDraws.size());
  int z = 0;
  for(auto const&x : numberDraws){
    output[z] = x.second;
    z++;
  }
  return output;
}


// [[Rcpp::export]]
std::vector<double> testLoop_cpp(std::vector<std::vector<int>> inputGroup,  std::vector<std::vector<double>> risk){
  
  int n = inputGroup.size();
  int nhier = inputGroup[0].size();
  
  std::map<std::vector<int>,std::unordered_set<int>> mapGroup;
  std::vector<int> hier_help(nhier);
  for(int i =0;i<n;i++){
    // ... define hierarchy group
    for(int j=0;j<nhier;j++){
      hier_help[j] = inputGroup[i][j];
    }
    
    // supply new household index to each group
    mapGroup[hier_help].insert(i);
  }
  
  // std::map<std::vector<int>,double > ratioRisk; // get ratio of numbers to draw in lowest level hierarchy
  std::map<std::vector<int>,double > sumRisk; // sum of Risk in each hierarchy level
  std::map<std::vector<int>,int > unitsHierarchy; // number of units in each hierarchy
  // double sumRatio = 0.0; //help variable for ratio
  // double helpRatio = 0.0; //help variable for ratio
  
  // calcualte sum of risk in each hierarchy level
  std::vector<int> maxIndex(nhier,0); 
  for(auto const&x : mapGroup){
    
    std::vector<int> hl = x.first;
    
    // for (auto const& i: hl) {
    //   std::cout << i << " ";
    // }
    // std::cout << "\n";
    
    for(int h=nhier; h-- >0; ){
      for (const auto& indexI: x.second){
        sumRisk[hl] += risk[indexI][h];
      }
      
      // for (auto const& i: hl) {
      //   std::cout << i << " ";
      // }
      
      maxIndex[h] = max(maxIndex[h],hl.back());
      
      hl.pop_back();

      // std::cout << "\n";
      
    }
  }
  
  std::vector<double> output(sumRisk.size());
  
  int z=0;
  for(auto const&x : sumRisk){
    output[z] = x.second;
    z++;
  } 

  return output;

}


/*
 * Some test functions
 * NOT USED AS OF RIGHT NOW!
 */
// [[Rcpp::export]]
std::vector<int> test_prioqueue(std::vector<int> x_vec,std::vector<double> prob,std::vector<int> mustSwap_vec,int n,int seed){

  std::unordered_set<int> mustSwap(mustSwap_vec.begin(),mustSwap_vec.end());
  std::unordered_set<int> x(x_vec.begin(),x_vec.end());
  
  std::priority_queue<std::pair<double, int> > q;
  std::vector<int> sampleID(x.size());
  std::exponential_distribution<double> exp_dist(1.0);
  std::mt19937 mersenne_engine;
  mersenne_engine.seed(seed);

  int z=0;
  for(auto s : x){
    if(mustSwap.find(s)!=mustSwap.end()){
      sampleID[z] = s;
      z++;
    }else{
      q.push(std::pair<double, int>(prob[s]/exp_dist(mersenne_engine), s));
    }
  }
  
  n = max(0,min<int>(q.size(),n-z));
  sampleID.resize(n+z);

  // select index of top elements from priority_queue
  for(int i=0;i<n;i++){
    sampleID[i+z] = q.top().second; //.top() access top element in queue
    q.pop(); // remove top element in queue
  }

  return sampleID;
}



struct Comp{
  Comp(const std::vector<double>& v ) : _v(v) {}
  // Inverted comparison!
  bool operator ()(int a, int b) { return _v[a] > _v[b]; }
  const std::vector<double>& _v;
};

// [[Rcpp::export]]
std::vector<int> test_comparator(std::vector<int> x_vec,std::vector<double> prob,std::vector<int> mustSwap_vec, int n, int seed){
  
  std::unordered_set<int> mustSwap(mustSwap_vec.begin(),mustSwap_vec.end());
  std::unordered_set<int> x(x_vec.begin(),x_vec.end());
  
  std::exponential_distribution<double> exp_dist(1.0);
  std::mt19937 mersenne_engine;
  mersenne_engine.seed(seed);
  // int N = x.size();
  std::vector<int> sampleID(x.begin(),x.end());

  // apply prob[i]/exp_dist(mersenne_engine) over whole prob vector
  std::for_each(prob.begin(),prob.end(), [&exp_dist, &mersenne_engine](double& d) {d = d/exp_dist(mersenne_engine);});
  // define highest sampling value and give all elements in mustSwap this probabilits (they will always be selected)

  // sort x by prob
  std::partial_sort(sampleID.begin(),sampleID.begin()+n,sampleID.end(),Comp(prob));

  // include must swapped values if necessary
  int z=0;
  for(auto s : mustSwap){
    if(x.find(s)!=x.end() && std::find(sampleID.begin(), sampleID.begin()+n, s) == sampleID.begin()+n){
      sampleID[n-(z+1)] = s;
      z++;
    }
  }

  sampleID.resize(n);
  return sampleID;
}

