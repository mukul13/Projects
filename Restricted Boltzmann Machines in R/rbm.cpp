// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <math.h>
#include <ctime>
using namespace Rcpp;

RCPP_EXPOSED_CLASS(rbm)

class rbm
{
public:
	long nhidden;
	long nvisible;
	double learning_rate;
	long max_epochs;

	arma::mat weights;		

rbm(long rvisible,long rhidden,double lr,long me)
{

	nhidden=rhidden;
	nvisible=rvisible;
	learning_rate=lr;
	max_epochs=me;

	weights.set_size(nvisible+1,nhidden+1);
	weights.fill(0);

	for(int i=1;i<=nhidden;i++)
	{
		weights.col(i)=as<arma::vec>(rnorm(nvisible+1,0.1));
		weights(0,i)=0;
	}
	//weights.print("weights");
}	

void train(arma::mat train_data)
{
	arma::mat bias;
	bias.set_size(train_data.n_rows,1);
	bias.fill(1);

	train_data=join_rows(bias,train_data);

for(int epoch=0;epoch<max_epochs;epoch++)
{	
	arma::mat pos_hidden_activations=train_data * weights;
	//pos_hidden_activations.print();
	arma::mat pos_hidden_probs=sigmoid_function(pos_hidden_activations);
	//pos_hidden_probs.print();
	arma::mat temp=arma::randu(train_data.n_rows*(nhidden+1));
	temp.reshape(train_data.n_rows,nhidden+1);
	
	arma::mat pos_hidden_states = get_states(pos_hidden_probs,temp);
	//pos_hidden_states.print();

	arma::mat pos_associations=trans(train_data)*pos_hidden_probs;
	//pos_associations.print();

	arma::mat neg_visible_activations=pos_hidden_states*trans(weights);
	arma::mat neg_visible_probs=sigmoid_function(neg_visible_activations);
	neg_visible_probs.col(0).fill(1);
	//neg_visible_probs.print();
	arma::mat neg_hidden_activations=neg_visible_probs*weights;
	arma::mat neg_hidden_probs=sigmoid_function(neg_hidden_activations);
	//train_data.print("train data");
	arma::mat neg_association=trans(neg_visible_probs)*neg_hidden_probs;

	temp=pos_associations-neg_association;	
	for(int i=0;i<(int)weights.n_rows;i++)
	{
		for(int j=0;j<(int)weights.n_cols;j++)
		{
			weights(i,j)+=learning_rate*temp(i,j)/train_data.n_rows;
		}
	}
	//weights.print();
	temp=train_data-neg_visible_probs;
	temp=pow(temp,2);

	double error=sum(sum(temp));
	Rcout<<"Epoch: "<<epoch<<" error: "<<error<<std::endl; 
}

}

NumericMatrix run_visible(arma::mat train_data)
{
	arma::mat bias;
	bias.set_size(train_data.n_rows,1);
	bias.fill(1);

	train_data=join_rows(bias,train_data);

	arma::mat hidden_activations=train_data*weights;
	arma::mat hidden_probs=sigmoid_function(hidden_activations);

	arma::mat temp=arma::randu(train_data.n_rows*(nhidden+1));
	temp.reshape(train_data.n_rows,nhidden+1);
	
	arma::mat hidden_states=get_states(hidden_probs,temp);
	hidden_states=hidden_states.submat(0,1,hidden_states.n_rows-1,hidden_states.n_cols-1);
	return wrap(hidden_states);
}	

NumericMatrix run_hidden(arma::mat train_data)
{
	arma::mat visible_states;
	visible_states.set_size(train_data.n_rows,nvisible);
	visible_states.fill(1);

	arma::mat bias;
	bias.set_size(train_data.n_rows,1);
	bias.fill(1);

	train_data=join_rows(bias,train_data);

	arma::mat visible_activations=train_data*trans(weights);
	arma::mat visible_probs=sigmoid_function(visible_activations);

	arma::mat temp=arma::randu(train_data.n_rows*(nvisible+1));
	temp.reshape(train_data.n_rows,nvisible+1);
	visible_states=	get_states(visible_probs,temp);
	visible_states=visible_states.submat(0,1,visible_states.n_rows-1,visible_states.n_cols-1);

	return wrap(visible_states);
}


NumericMatrix daydream(long num)
{
	arma::mat samples;
	samples.set_size(num,nvisible+1);
	samples.fill(1);
	samples.row(0)=arma::randu(nvisible+1);
	samples(0,0)=1;

	for(int i=1;i<num;i++)
	{
		arma::mat visible=samples.row(i-1);
		visible.print("print");
		arma::mat hidden_activations=visible*weights;
		hidden_activations.print("hidden_activations");
		arma::mat hidden_probs=sigmoid_function(hidden_activations);
		hidden_probs.print("hidden_probs");
		arma::rowvec temp=arma::randu(nhidden+1);	
		arma::rowvec hidden_states=get_states(hidden_probs,temp);

		hidden_states(0)=1;

		arma::rowvec visible_activations=hidden_states*trans(weights);

		arma::rowvec visible_probs=sigmoid_function(visible_activations);
		temp=arma::randu(nvisible+1);
		arma::rowvec visible_states=get_states(visible_probs,temp);
		samples.row(i)=visible_states;		
	}
	samples.print("samples");
return wrap(samples);	
}

arma::mat get_states(arma::mat x,arma::mat y)
{
	arma::mat out;
	out.set_size(x.n_rows,x.n_cols);

	for(int i=0;i<(int)x.n_rows;i++)
	{
		for(int j=0;j<(int)x.n_cols;j++)
		{
			if(x(i,j)>y(i,j))out(i,j)=1;
			else out(i,j)=0;	
		}
	}
	return out;
}


arma::mat sigmoid_function( arma::mat x )
{
		//sigmoid function
		return 1/(1+exp(-x));
}

void save_weights()
{
	weights.save("weights.txt", arma::raw_ascii);
}

void load_weights()
{
	weights.load("weights.txt");
}


};

RCPP_MODULE(restricted_boltzmann_machine)
{
	class_<rbm>("rbm")
	.constructor<long,long,double,long>()
	.method("train",&rbm::train)
	.method("run_visible",&rbm::run_visible)
	.method("run_hidden",&rbm::run_hidden)
	.method("daydream",&rbm::daydream)
	.method("save_weights",&rbm::save_weights)
	.method("load_weights",&rbm::load_weights)
	;
}