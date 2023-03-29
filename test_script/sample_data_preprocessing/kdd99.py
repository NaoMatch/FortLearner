import pandas as pd
import requests
import subprocess
from sklearn.preprocessing import LabelEncoder, OneHotEncoder
from sklearn.model_selection import StratifiedKFold

def download_kdd99_10_percent(output_path):
    url = "https://archive.ics.uci.edu/ml/machine-learning-databases/kddcup99-mld/kddcup.data_10_percent.gz"

    fn_gz = "kddcup.data_10_percent.gz"
    fn_unzip = "kddcup.data_10_percent"

    file_name = output_path + fn_gz

    print(f"Downloading from {url}.")
    with open(file_name, "wb") as f:
        r = requests.get(url)
        f.write(r.content)

    print(f"Unzipping {file_name}.")
    subprocess.run(f"gunzip -f {file_name}", shell=True, check=True)

    return f"{output_path}/{fn_unzip}"
    
def download_kdd99(output_path):
    url = "https://archive.ics.uci.edu/ml/machine-learning-databases/kddcup99-mld/kddcup.data.gz"

    fn_gz = "kddcup.data.gz"
    fn_unzip = "kddcup.data"

    file_name = output_path + fn_gz

    print(f"Downloading from {url}.")
    with open(file_name, "wb") as f:
        r = requests.get(url)
        f.write(r.content)

    print(f"Unzipping {file_name}.")
    subprocess.run(f"gunzip -f {file_name}", shell=True, check=True)

    return f"{output_path}/{fn_unzip}"
    
def preprocessing_kdd99(file_name):
    """
        https://openreview.net/forum?id=BJJLHbb0-

        '''
        The KDDCUP99 10 percent dataset from the UCI repository (Lichman (2013))
        originally contains samples of 41 dimensions, where 34 of them are continuous and 7 are
        categorical. For categorical features, we further use one-hot representation to encode them,
        and eventually we obtain a dataset of 120 dimensions. As 20% of data samples are labeled
        as “normal” and the rest are labeled as “attack”, “normal” samples are in a minority group;
        therefore, “normal” ones are treated as anomalies in this task.
        '''

        １．normalを異常グループとする。
        ２．OneHotEncodeだけでなく、LabelEncodeを実施する
    """
    column_names = [
        'duration',
        'protocol_type',
        'service',
        'flag',
        'src_bytes',
        'dst_bytes',
        'land',
        'wrong_fragment',
        'urgent',
        'hot',
        'num_failed_logins',
        'logged_in',
        'num_compromised',
        'root_shell',
        'su_attempted',
        'num_root',
        'num_file_creations',
        'num_shells',
        'num_access_files',
        'num_outbound_cmds',
        'is_host_login',
        'is_guest_login',
        'count',
        'srv_count',
        'serror_rate',
        'srv_serror_rate',
        'rerror_rate',
        'srv_rerror_rate',
        'same_srv_rate',
        'diff_srv_rate',
        'srv_diff_host_rate',
        'dst_host_count',
        'dst_host_srv_count',
        'dst_host_same_srv_rate',
        'dst_host_diff_srv_rate',
        'dst_host_same_src_port_rate',
        'dst_host_srv_diff_host_rate',
        'dst_host_serror_rate',
        'dst_host_srv_serror_rate',
        'dst_host_rerror_rate',
        'dst_host_srv_rerror_rate',
        'outcome'
    ]    
    def convert(x):
        if x=="normal.":
            return 1
        return 0
    print(f"Reading {file_name}")
    xy = pd.read_csv(file_name, header=None)
    xy.columns = column_names

    print(f"Convert 'normal' as anomalous, rest are normal.")
    xy["outcome"] = [convert(x) for x in  xy["outcome"]]

    print(f"Feature Encoding. {['protocol_type', 'service', 'flag', 'su_attempted']}")
    enc_target_columns = ["protocol_type", "service", "flag", "su_attempted"]
    xy_l_enc = xy.copy()
    xy_o_enc = xy.copy()
    for enc_col in enc_target_columns:
        l_enc = LabelEncoder()
        xy_l_enc[enc_col] = l_enc.fit_transform(xy_l_enc[enc_col])
            
        df_enc_col = pd.get_dummies(xy[[enc_col]].astype(str))
        del xy_o_enc[enc_col]
        xy_o_enc = pd.concat([xy_o_enc, df_enc_col], axis=1)

    fn_out_label_enc = f'{file_name}_label_enc.csv'
    print(f"Saving {fn_out_label_enc}")
    xy_l_enc.to_csv(fn_out_label_enc, index=False)

    fn_out_one_hot_enc = f'{file_name}_one_hot_enc.csv'
    print(f"Saving {fn_out_one_hot_enc}")
    xy_o_enc.to_csv(fn_out_one_hot_enc, index=False)

    return fn_out_label_enc, fn_out_one_hot_enc


def split_train_valid_test_kdd99(file_name):
    xy = pd.read_csv(file_name)

    x = xy.copy(); del x["outcome"]
    y = xy[["outcome"]].copy()

    random_idx = np.random.permutation(list(range(len(xy))))
    x = x.iloc[random_idx,:]
    y = y.iloc[random_idx,:]

    subprocess.run(f"./main_csv2bin.out test  {N_TEST}  {N_FEATURES} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} r r", shell=True, check=True)
