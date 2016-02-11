// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <math.h>
#include <ctime>
using namespace Rcpp;

RCPP_EXPOSED_CLASS(neural_network)

class neural_network
{
public:	
	long  ninput;
	long nhidden;
	long noutput;
	
//neurons
	arma::rowvec input_neurons;
	arma::rowvec hidden_neurons;
	arma::rowvec output_neurons;

	//weights
	arma::mat w_input_hidden;
	arma::mat w_hidden_output;

	//epoch counter
	long epoch;
	long  max_epochs;
	
	double learning_rate;
	double momentum;
	bool use_batch;
	//accuracy required
	double desired_accuracy;

	//change to weights
	arma::mat delta_input_hidden;
	arma::mat delta_hidden_output;

	//error gradients
	arma::rowvec hidden_error_gradients;
	arma::rowvec output_error_gradients;
	
	double training_set_accuracy;
	double validation_set_accuracy;
	double generalization_set_accuracy;
	double training_MSE;
	double validation_MSE;
	double generalization_MSE;

void set_max_epochs(long me)
{
	max_epochs=me;
}

void batch_setting(bool ub)
{
	use_batch=ub;
}

void set_learning_parameters(double lr,double mom)
{
	learning_rate=lr;
	momentum=mom;
}

neural_network(long long rinput,long long rhidden,long long routput)
{
	// to get size of input,hidden and output layers
	ninput=rinput-1;
	nhidden=rhidden;
	noutput=routput;
	learning_rate=0.001;
	momentum=0.01;
	use_batch=true;
	max_epochs=1000;
	desired_accuracy=90;
	
	
	input_neurons.set_size(ninput+1);
	input_neurons.fill(0);
	input_neurons(ninput)=-1;

	hidden_neurons.set_size(nhidden+1);
	hidden_neurons.fill(0);
	hidden_neurons(nhidden)=-1;

	output_neurons.set_size(noutput+1);
	output_neurons.fill(0);
	output_neurons(noutput)=-1;

	delta_input_hidden.set_size(ninput+1,nhidden);
	delta_hidden_output.set_size(nhidden+1,noutput);
	delta_input_hidden.fill(0);
	delta_hidden_output.fill(0);

	w_input_hidden=arma::randu((ninput+1)*nhidden)-0.5;
	w_input_hidden.reshape(ninput+1, nhidden);
	//w_input_hidden.print("mat");
	w_hidden_output=arma::randu((nhidden+1)*noutput)-0.5;
	w_hidden_output.reshape(nhidden+1, noutput);

	hidden_error_gradients.set_size(nhidden+1);
	hidden_error_gradients.fill(0);

	output_error_gradients.set_size(noutput+1);
	output_error_gradients.fill(0);
	

//input_neurons.print();
//	Rcout<<train.n_rows<<std::endl;
//	Rcout<<train.n_cols<<std::endl;
//	Rcout<<train(1,1)<<std::endl;
//	train(1,1)=3;
//	Rcout<<train(1,1)<<std::endl;

}

void train_network(List train,List gen,List val)
	{
	arma::mat train_data;
	arma::mat gen_data;
	arma::mat val_data;
	arma::mat train_target;
	arma::mat gen_target;
	arma::mat val_target;

//	NumericMatrix train_temp=as<NumericMatrix>(train["data"]);
	train_data=as<arma::mat>(as<NumericMatrix>(train["data"]));
	train_target=as<arma::mat>(as<NumericMatrix>(train["label"]));
	gen_data=as<arma::mat>(as<NumericMatrix>(gen["data"]));
	gen_target=as<arma::mat>(as<NumericMatrix>(gen["label"]));
	val_data=as<arma::mat>(as<NumericMatrix>(val["data"]));
	val_target=as<arma::mat>(as<NumericMatrix>(val["label"]));
	
	
	//Rcout<<train_data.row(0);	
	//train_data.print();
	//gen_data=as<arma::mat>(gen);
	//val_data=as<arma::mat>(val);

		Rcout<< std::endl << " Neural Network Training Starting: " << std::endl
			<< "==========================================================================" << std::endl
			<< " LR: " << learning_rate << ", Momentum: " << momentum << ", Max Epochs: " << max_epochs << std::endl
			<< " " << ninput << " Input Neurons, " << nhidden << " Hidden Neurons, " << noutput << " Output Neurons" << std::endl
			<< "==========================================================================" << std::endl << std::endl;

		epoch = 0;
		training_set_accuracy=0;
		generalization_set_accuracy=0;
			
		//train network using training dataset for training and generalization dataset for testing
		//--------------------------------------------------------------------------------------------------------
		while (	( training_set_accuracy < desired_accuracy || generalization_set_accuracy < desired_accuracy ) && epoch < max_epochs )				
		{			
			//store previous accuracy
			double previous_T_accuracy = training_set_accuracy;
			double previous_G_accuracy = generalization_set_accuracy;

			//use training set to train network
			run_training_epoch( train_data ,train_target);

			//get generalization set accuracy and MSE
			generalization_set_accuracy = get_set_accuracy( gen_data,gen_target );
			generalization_MSE = get_set_mse( gen_data,gen_target);

			
			//print out change in training /generalization accuracy (only if a change is greater than a percent)
			if ( ceil(previous_T_accuracy) != ceil(training_set_accuracy) || ceil(previous_G_accuracy) != ceil(generalization_set_accuracy) ) 
			{	
				Rcout << "Epoch :" << epoch;
				Rcout << " TSet Acc:" << training_set_accuracy << "%, MSE: " << training_MSE ;
				Rcout << " GSet Acc:" << generalization_set_accuracy << "%, MSE: " << generalization_MSE << std::endl;				
			}
			
			//once training set is complete increment epoch
			epoch++;

		}

		//get validation set accuracy and MSE
		validation_set_accuracy = get_set_accuracy(val_data,val_target);
		validation_MSE = get_set_mse(val_data,val_target);

			
		//out validation accuracy and MSE
		Rcout << std::endl << "Training Complete!!! - > Elapsed Epochs: " << epoch << std::endl;
		Rcout << " Validation Set Accuracy: " << validation_set_accuracy << std::endl;
		Rcout << " Validation Set MSE: " << validation_MSE << std::endl << std::endl;
		
	}		

void run_training_epoch(arma::mat training_set,arma::mat target)
	{
		//incorrect patterns
		double incorrect_patterns = 0;
		double mse = 0;
		
		//for every training pattern//
		for ( int tp = 0; tp < (int)training_set.n_rows ; tp++)
		{						
			//feed inputs through network and backpropagate errors
			feed_forward( training_set.row(tp) );
			backpropagate(target.row(tp) );	


			//pattern correct flag
			bool pattern_correct = true;

			//check all outputs from neural network against desired values
			for ( int k = 0; k < noutput; k++ )
			{					
				//pattern incorrect if desired and output differ
				if ( get_rounded_output_value( output_neurons[k] ) != as<double>(wrap(target(tp,k))))  pattern_correct = false;
				
				//calculate MSE
				mse += (output_neurons[k] - as<double>(wrap(target(tp,k))))*(output_neurons[k] - as<double>(wrap(target(tp,k))));
			}
			
			//if pattern is incorrect add to incorrect count
			if ( !pattern_correct ) incorrect_patterns++;	
			
		}//end for

		//if using batch learning - update the weights
		if ( use_batch ) update_weights();
		
		//update training accuracy and MSE
		training_set_accuracy = 100 - (incorrect_patterns/training_set.n_rows * 100);
		training_MSE = mse / ( noutput * training_set.n_rows );
	//	Rcout<<training_set_accuracy<<"   "<<training_MSE;
	}

void feed_forward(arma::rowvec inputs)
	{
		//set input neurons to input values
	//	inputs.print();
		for(int i=0;i<=ninput;i++)
			input_neurons(i)=inputs(i);

		//input_neurons.print();
		//Calculate Hidden Layer values - include bias neuron
		//--------------------------------------------------------------------------------------------------------
			//clear value
		hidden_neurons.fill(0);
		
		for(int j=0; j < nhidden; j++)
		{
			
			//get weighted sum of inputs and bias neuron
			for( int i=0; i <= ninput; i++ ) hidden_neurons(j) += input_neurons(i) * w_input_hidden(i,j);

			//set to result of sigmoid
			hidden_neurons(j) = activation_function( as<double>(wrap(hidden_neurons(j))));			
		}
	//	hidden_neurons.print("hidden neurons");
		//Calculating Output Layer values - include bias neuron
		//--------------------------------------------------------------------------------------------------------
		//output_neurons.fill(0);
		//d=trans(w_hidden_output);
		//d1=sum(hidden_neurons % d.each_row() ,1);
		//d1.print();
		//d1=trans(d1);
		//d2=wrap(d1);
		//output_neurons=as<arma::rowvec>(d2);
		//output_neurons.print("output neurons");
		//output_neurons=d2;
		//output_neurons.print("output neurons");
		//output_neurons.fill(0);
		for(int k=0; k < noutput; k++)
		{

			
			//get weighted sum of inputs and bias neuron
			for( int j=0; j <= nhidden; j++ ) output_neurons(k) += hidden_neurons(j) * w_hidden_output(j,k);
			
			//set to result of sigmoid
			output_neurons(k) = activation_function(as<double>(wrap(output_neurons(k))));
		}
		//output_neurons.print("output neurons");
	}

void backpropagate( arma::rowvec desired)
	{		
		//modify deltas between hidden and output layers
		//--------------------------------------------------------------------------------------------------------
		for (int k = 0; k < noutput; k++)
		{
			//get error gradient for every output node
			output_error_gradients[k] = get_output_error_gradient( as<double>(wrap(desired(k))), output_neurons(k));
			
			//for all nodes in hidden layer and bias neuron
			for (int j = 0; j <= nhidden; j++) 
			{				
				//calculate change in weight
				if ( !use_batch ) delta_hidden_output(j,k) = learning_rate * hidden_neurons(j) * output_error_gradients(k) + momentum * delta_hidden_output(j,k);
				else delta_hidden_output(j,k) += learning_rate * hidden_neurons(j) * output_error_gradients(k);
			}
		}

		//modify deltas between input and hidden layers
		//--------------------------------------------------------------------------------------------------------
		for (int j = 0; j < nhidden; j++)
		{
			//get error gradient for every hidden node
			hidden_error_gradients(j) = get_hidden_error_gradient( j );

			//for all nodes in input layer and bias neuron
			for (int i = 0; i <= ninput; i++)
			{
				//calculate change in weight 
				if ( !use_batch ) delta_input_hidden(i,j) = learning_rate * input_neurons(i) * hidden_error_gradients(j) + momentum * delta_input_hidden(i,j);
				else delta_input_hidden(i,j) += learning_rate * input_neurons(i) * hidden_error_gradients(j); 

			}
		}
		
		//if using stochastic learning update the weights immediately
		if ( !use_batch ) update_weights();
	}


void update_weights()
	{
		//input -> hidden weights
		//--------------------------------------------------------------------------------------------------------
		for (int i = 0; i <= ninput; i++)
		{
			for (int j = 0; j < nhidden; j++) 
			{
				//update weight
				w_input_hidden(i,j) += delta_input_hidden(i,j);	
				
				//clear delta only if using batch (previous delta is needed for momentum
				if (use_batch) delta_input_hidden(i,j) = 0;				
			}
		}
		
		//hidden -> output weights
		//--------------------------------------------------------------------------------------------------------
		for (int j = 0; j <= nhidden; j++)
		{
			for (int k = 0; k < noutput; k++) 
			{					
				//update weight
				w_hidden_output(j,k) += delta_hidden_output(j,k);
				
				//clear delta only if using batch (previous delta is needed for momentum)
				if (use_batch)delta_hidden_output(j,k) = 0;
			}
		}
	}

double activation_function( double x )
	{
		//sigmoid function
		return 1/(1+exp(-x));
	}

double get_output_error_gradient(double desiredValue, double outputValue)
	{
		//return error gradient
		return outputValue * ( 1 - outputValue ) * ( desiredValue - outputValue );
	}

	//get error gradient for hidden layer
double get_hidden_error_gradient( int j )
{
		//get sum of hidden->output weights * output error gradients
		double weighted_sum = 0;
		for( int k = 0; k < noutput; k++ ) weighted_sum += w_hidden_output(j,k) * output_error_gradients(k);

		//return error gradient
		return hidden_neurons(j) * ( 1 - hidden_neurons(j) ) * weighted_sum;
}

	//round up value to get a boolean result
int get_rounded_output_value( double x )
{
		if ( x < 0.5 ) return 0;
		//else if ( x > 0.9 ) return 1;
		else return 1;
}	


double get_set_accuracy( arma::mat set,arma::mat target )
	{
		double incorrect_results = 0;
		
		//for every training input array
		for ( int tp = 0; tp < (int)set.n_rows; tp++)
		{						
			//feed inputs through network and backpropagate errors
			feed_forward( set.row(tp));
			
			//correct pattern flag
			bool correct_result = true;

			//check all outputs against desired output values
			for ( int k = 0; k < noutput; k++ )
			{					
				//set flag to false if desired and output differ
				if ( get_rounded_output_value(output_neurons[k]) != as<double>(wrap(target(tp,k)))) correct_result = false;
			}
			
			//inc training error for a incorrect result
			if ( !correct_result ) incorrect_results++;	
			
		}//end for
		
		//calculate error and return as percentage
		return 100 - (incorrect_results/set.n_rows * 100);
	}

	//feed forward set of patterns and return MSE
	double get_set_mse ( arma::mat set,arma::mat target)
	{
		double mse = 0;
		
		//for every training input array
		for ( int tp = 0; tp < (int)set.n_rows; tp++)
		{						
			//feed inputs through network and backpropagate errors
			feed_forward( set.row(tp));
			
			//check all outputs against desired output values
			for ( int k = 0; k < noutput; k++ )
			{					
				//sum all the MSEs together
				mse += pow((output_neurons[k] - as<double>(wrap(target(tp,k)))), 2);
			}		
			
		}//end for
		
		//calculate error and return as percentage
		return mse/(noutput * set.size());
	}

NumericMatrix predict(arma::mat set)
{

	arma::mat pred;
	pred.set_size(set.n_rows,noutput);
	for ( int tp = 0; tp < (int)set.n_rows; tp++)
		{						
			//feed inputs through network and backpropagate errors
			feed_forward( set.row(tp));
			

			for ( int k = 0; k < noutput; k++ )
			{					
				pred(tp,k)=get_rounded_output_value(output_neurons(k));
			}
			
			
		}

	return wrap(pred);		
}

void save_weights()
{
	w_input_hidden.save("w_input_hidden.txt", arma::raw_ascii);
	w_hidden_output.save("w_hidden_output.txt",arma::raw_ascii);
}

void load_weights()
{
	w_input_hidden.load("w_input_hidden.txt");
	w_hidden_output.load("w_hidden_output.txt");
}

};

RCPP_MODULE(nn)
{
	class_<neural_network>("neural_network")
	.constructor<long,long,long>()
	.method("set_learning_parameters",&neural_network::set_learning_parameters)
	.method("train_network",&neural_network::train_network)
	.method("predict",&neural_network::predict)
	.method("save_weights",&neural_network::save_weights)
	.method("set_max_epochs",&neural_network::set_max_epochs)
	.method("load_weights",&neural_network::load_weights)
	.method("batch_setting",&neural_network::batch_setting)
	;
}
