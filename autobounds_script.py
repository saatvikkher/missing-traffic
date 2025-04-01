# load autobounds components used in this demo
from autobounds.causalProblem import causalProblem
from autobounds.DAG import DAG
from autobounds.Query import Query

# load additional dependencies
import pandas as pd
import statsmodels.formula.api as smf
import plotnine as pn
import numpy as np

# Warning surpress
import sys
import os
from contextlib import contextmanager
import statsmodels.formula.api as smf
import warnings

# configure plotting options
pn.options.figure_size = (8, 4)

@contextmanager
def suppress_stdout():
    with open(os.devnull, 'w') as devnull:
        old_stdout = sys.stdout
        sys.stdout = devnull
        try:
            yield
        finally:
            sys.stdout = old_stdout


def calculate_bounds(data, query_threshold = 0.32):
    model = smf.ols("Y ~ D", data = data).fit()

    # count units with each unique combination of Z, X, Y 
    data_summary = (
        data
        .loc[:, ['D', 'M', 'Y']]
        .value_counts()
        .rename('counts')
        .reset_index()
    )

    # divide by the total to get the estimated probability of each type
    data_summary = data_summary.assign(prob = data_summary.counts / data_summary.counts.sum())


    # first initialize an empty graph
    graph = DAG()

    # define edges and unobserved disturbances
    graph.from_structure(
        edges = "D -> M, D -> Y, M -> Y, U -> M, U -> Y",
        unob = "U"
    )

    # initialize a causal-inference problem involving the klm dag
    problem = causalProblem(
        graph,
        number_values = {"D" : 2, "M" : 2, "Y" : 2}  # for illustration (not needed, same as defaults)
    )

    # assumption 1: if no stop is made, no force is used
    force_used_without_stop = problem.query('Y(M=0)=1')    # define the group
    problem.add_constraint(force_used_without_stop, '==')  # constrain group size (defaults to 0)

    # assumption 2: if officers discriminate in stopping at all, it is likely against black civilians
    anti_white_stop = problem.query("M(D=0)=1 & M(D=1)=0")  # define the group
    problem.add_constraint(anti_white_stop, '==')           # constrain group size (defaults to 0)

    # assumption 3: 

    # define groups (anti-white stops are defined above)
    always_stop = problem.query("M(D=0)=1 & M(D=1)=1")
    anti_black_stop = problem.query("M(D=0)=0 & M(D=1)=1")

    # for all d and m
    for d in [0, 1]:
        for m in [0, 1]:

            # Pr[ force would counterfactually be used under this d and m AND encounter belongs to this group ]
            potential_force_dm_and_always_stop = problem.query("Y(D={d},M={m})=1 & M(D=0)=1 & M(D=1)=1".format(d=d, m=m))
            potential_force_dm_and_anti_black_stop = problem.query("Y(D={d},M={m})=1 & M(D=0)=0 & M(D=1)=1".format(d=d, m=m))
            potential_force_dm_and_anti_white_stop = problem.query("Y(D={d},M={m})=1 & M(D=0)=1 & M(D=1)=0".format(d=d, m=m))

            # using functionality that is currently available, the following is an equivalent but slightly more clunky workaround
            problem.add_constraint(
                potential_force_dm_and_anti_black_stop * always_stop - potential_force_dm_and_always_stop * anti_black_stop,
                "<=" 
                # defaults to 0
            )
            problem.add_constraint(
                potential_force_dm_and_anti_white_stop * always_stop - potential_force_dm_and_always_stop * anti_white_stop,
                "<=" 
                # defaults to 0
            )

    # incorporating side information: proportion of black stops that would not have been made if white
    black_and_stop = problem.query("D=1 & M=1")
    black_and_stop_and_discriminatory = problem.query("D=1 & M=1 & M(D=0)=0")
    lower_bound_on_prop_black_stops_discriminatory_gfk07 = Query(query_threshold)

    # using functionality that is currently available, the following is an equivalent but slightly more clunky workaround
    problem.add_constraint(
        black_and_stop_and_discriminatory - 
        lower_bound_on_prop_black_stops_discriminatory_gfk07 * black_and_stop,
        "=="
        # defaults to 0
    )

    # tell autobounds about the laws of probability
    problem.add_prob_constraints()

    # load in the data
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", FutureWarning)
        problem.load_data(data_summary, cond = ["M"])

    # for numerical stability, currently we need to add a constraint that the subgroup of interest has nonzero size
    # this will be made automatic in a future version
    stop = problem.query("M=1")
    small_positive_number = Query(.01)
    problem.add_constraint(stop - small_positive_number, ">=")  # defaults to 0

    problem.set_ate(ind = "D", dep = "Y", cond = "M=1")

    # translate causal inference problem into optimization program
    program = problem.write_program()

    program.to_pip('klm_baseline.pip')
    
    with suppress_stdout():
        results = program.run_scip(verbose = False, epsilon = .1)
    
    # Return the calculated bounds and naive estimate
    lower_bound = results[0]["dual"]
    upper_bound = results[1]["dual"]
    naive_estimate = model.params.D
    
    return lower_bound, upper_bound, naive_estimate

def simulate_missingness(data, prop = [0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0], rho = [0.32]):

    # Get the indices of NaN values in column D
    nan_indices = data[data['D'].isna()].index

    # Dataframe to store the results
    results_df = pd.DataFrame(columns=['prop', 'naive_estimate', 'lower_bound', 'upper_bound', 'query_threshold'])
    
    # iterate over the queries and steps
    for threshold in rho:
        for step in prop:
            # Determine the number of NaNs to replace for the current step
            num_nans = len(nan_indices)
            num_zeros = int(num_nans * step)
            num_ones = num_nans - num_zeros

            # Shuffle the indices to randomly select NaNs for replacement
            shuffled_indices = list(nan_indices)
            np.random.shuffle(shuffled_indices)

            # Assign 0s and 1s based on the current step
            zero_indices = shuffled_indices[:num_zeros]
            one_indices = shuffled_indices[num_zeros:]

            data.loc[zero_indices, 'D'] = 0
            data.loc[one_indices, 'D'] = 1

            # Calculate bounds for this step
            lower_bound, upper_bound, naive_estimate = calculate_bounds(data, query_threshold = threshold)

            # Create a temporary DataFrame for the current step
            temp_df = pd.DataFrame({
                'prop': [step],
                'naive_estimate': [naive_estimate],
                'lower_bound': [lower_bound],
                'upper_bound': [upper_bound],
                'query_threshold': [threshold]
            })

            # Append the results using pd.concat()
            results_df = pd.concat([results_df, temp_df], ignore_index=True)
            print(f"Step {int(step * 100)}% completed: {num_zeros} NaNs set to 0, {num_ones} NaNs set to 1")
            print(f"Updated value counts: \n {data.value_counts()}")
        
        print(f"Threshold: {threshold} completed")

    return results_df

def read_dataset(path):
    # read dataset
    data = pd.read_csv(path)

    # Filter the dataframe
    data = data[(data['subject_race'].isin(['white', 'black'])) | (data['subject_race'].isna())]

    # Select specific columns
    data = data[['subject_race', 'search_conducted']]

    # Create new column D using apply to handle NaN explicitly
    data['D'] = data['subject_race'].apply(lambda x: 0 if x == 'white' 
                                        else (1 if x == 'black' else np.nan))

    # Create M and Y columns
    data['M'] = 1
    data['Y'] = np.where(data['search_conducted'], 1, 0)

    # Select final columns
    data = data[['D', 'M', 'Y']]

    return data
def simulate_manski(data, rho=[0.32]):
    # Get the indices of NaN values in column D and True searches
    nan_true_indices = data[(data['D'].isna()) & (data['Y'] == 1)].index
    
    # Get the indices of NaN values in column D and False searches
    nan_false_indices = data[(data['D'].isna()) & (data['Y'] == 0)].index

    # Dataframe to store the results
    results_df = pd.DataFrame(columns=['prop', 'naive_estimate', 'lower_bound', 'upper_bound', 'query_threshold', 'anti_black_bias'])
    
    # iterate over the queries and steps
    for threshold in rho:
        for anti_black_bias in [0, 1]:
            # calculate prop NA assigned to white
            prop = len(nan_false_indices)/(len(nan_false_indices)+len(nan_true_indices)) if anti_black_bias else len(nan_true_indices)/(len(nan_false_indices)+len(nan_true_indices))
            data.loc[nan_true_indices, 'D'] = anti_black_bias
            data.loc[nan_false_indices, 'D'] = int(not anti_black_bias)
            
            lower_bound, upper_bound, naive_estimate = calculate_bounds(data, query_threshold = threshold)
            
            # Create a temporary DataFrame for the current step
            temp_df = pd.DataFrame({
                'prop': [prop],
                'naive_estimate': [naive_estimate],
                'lower_bound': [lower_bound],
                'upper_bound': [upper_bound],
                'query_threshold': [threshold],
                'anti_black_bias': [bool(anti_black_bias)]
            })

            # Append the results using pd.concat()
            results_df = pd.concat([results_df, temp_df], ignore_index=True)
            print(f"Step {int(prop * 100)}% completed: {len(nan_true_indices)} NaNs set to {anti_black_bias}, {len(nan_false_indices)} NaNs set to {int(not anti_black_bias)}")
            print(f"Updated value counts: \n {data.value_counts()}")
        
        print(f"Threshold: {threshold} completed")

    return results_df


if __name__ == "__main__":
    # List of dataset paths
    datasets = ['data/oh_statewide_2020_04_01.csv', 
                'data/co_statewide_2020_04_01.csv', 
                'data/wi_statewide_2020_04_01.csv',
                'data/wa_statewide_2020_04_01.csv',
                'data/md_statewide_2020_04_01.csv']

    rho = [0.25, 0.5, 0.75]

    # Use lists to accumulate results
    base_list = []
    missingness_list = []
    manski_list = []

    for path in datasets:
        # Extract dataset name uniformly for all results
        dataset_name = path.split('/')[-1]
        print(f"Processing dataset: {dataset_name}")
        data = read_dataset(path)

        # Calculate base bounds for each threshold in rho
        for threshold in rho:
            base_lower_bound, base_upper_bound, base_naive_estimate = calculate_bounds(data, threshold)
            temp_df = pd.DataFrame({
                'base_lower_bound': [base_lower_bound],
                'base_upper_bound': [base_upper_bound],
                'base_naive_estimate': [base_naive_estimate],
                'query_threshold': [threshold],
                'dataset_name': [dataset_name]
            })
            base_list.append(temp_df)

        # Simulate missingness and add dataset name (using only the filename)
        results_df = simulate_missingness(data, prop=[0, 0.25, 0.50, 0.75, 1.0], rho=rho)
        results_df['dataset_name'] = dataset_name
        missingness_list.append(results_df)

        # Simulate Manski bounds and add dataset name
        manski_df = simulate_manski(data, rho=rho)
        manski_df['dataset_name'] = dataset_name
        manski_list.append(manski_df)

    # Concatenate lists into final DataFrames
    base_df = pd.concat(base_list, ignore_index=True)
    missingness_results = pd.concat(missingness_list, ignore_index=True)
    manski_results = pd.concat(manski_list, ignore_index=True)

    # Save final results to CSV (writing once per output)
    base_df.to_csv('ATE_results_base.csv', index=False)
    missingness_results.to_csv('ATE_results.csv', index=False)
    manski_results.to_csv('ATE_results_manski.csv', index=False)
