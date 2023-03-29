import pandas as pd
import requests
import subprocess
import os
import mat73
import scipy.io
from sklearn.model_selection import StratifiedKFold
import urllib.request

odds_urls = [
        "https://www.dropbox.com/s/aifk51owxbogwav/annthyroid.mat?dl=0", 
        "https://www.dropbox.com/s/lmlwuspn1sey48r/arrhythmia.mat?dl=0", 
        "https://www.dropbox.com/s/g3hlnucj71kfvq4/breastw.mat?dl=0",
        "https://www.dropbox.com/s/galg3ihvxklf0qi/cardio.mat?dl=0",
        "https://www.dropbox.com/s/awx8iuzbu8dkxf1/cover.mat?dl=0",
        "https://www.dropbox.com/s/iq3hjxw77gpbl7u/glass.mat?dl=0",
        "https://www.dropbox.com/s/iy9ucsifal754tp/http.mat?dl=0",
        "https://www.dropbox.com/s/lpn4z73fico4uup/ionosphere.mat?dl=0",
        "https://www.dropbox.com/s/rt9i95h9jywrtiy/letter.mat?dl=0",
        "https://www.dropbox.com/s/ag469ssk0lmctco/lympho.mat?dl=0",
        "https://www.dropbox.com/s/tq2v4hhwyv17hlk/mammography.mat?dl=0",
        "https://www.dropbox.com/s/n3wurjt8v9qi6nc/mnist.mat?dl=0",
        "https://www.dropbox.com/s/we6aqhb0m38i60t/musk.mat?dl=0",
        "https://www.dropbox.com/s/w52ndgz5k75s514/optdigits.mat?dl=0",
        "https://www.dropbox.com/s/1x8rzb4a0lia6t1/pendigits.mat?dl=0",
        "https://www.dropbox.com/s/mvlwu7p0nyk2a2r/pima.mat?dl=0",
        "https://www.dropbox.com/s/dpzxp8jyr9h93k5/satellite.mat?dl=0",
        "https://www.dropbox.com/s/hckgvu9m6fs441p/satimage-2.mat?dl=0",
        "https://www.dropbox.com/s/mk8ozgisimfn3dw/shuttle.mat?dl=0",
        "https://www.dropbox.com/s/dbv2u4830xri7og/smtp.mat?dl=0",
        "https://www.dropbox.com/s/w6xv51ctea6uauc/speech.mat?dl=0",
        "https://www.dropbox.com/s/bih0e15a0fukftb/thyroid.mat?dl=0",
        "https://www.dropbox.com/s/5kuqb387sgvwmrb/vertebral.mat?dl=0",
        "https://www.dropbox.com/s/pa26odoq6atq9vx/vowels.mat?dl=0",
        "https://www.dropbox.com/s/ebz9v9kdnvykzcb/wbc.mat?dl=0",
        "https://www.dropbox.com/s/uvjaudt2uto7zal/wine.mat?dl=0",
    ]

def download_odds(output_path):
    global odds_urls

    for odds_url in odds_urls:
        base_fn = os.path.basename(odds_url).replace("?dl=0", "")
        output_file_name = output_path + "/" + base_fn

        print(f"Downloading from {odds_url}.")
        urllib.request.urlretrieve(odds_url, output_file_name)
        split_train_valid_test_odds(output_file_name)
        print(f"    done.")
        break
        
def split_train_valid_test_odds(file_name):

    try:
        data_dict = scipy.io.loadmat(file_name)
    except:
        data_dict = mat73.loadmat(file_name)

    x = pd.DataFrame(data_dict["X"])
    y = pd.DataFrame(data_dict["y"])
    ext = ".mat"

    np.seed(42)
    random_idx = np.random.permutation(list(range(len(x))))
    x = x.iloc[random_idx,:]
    y = y.iloc[random_idx,:]

    skf = StratifiedKFold(n_splits=5)
    all_indices = None
    for train_index, test_index in skf.split(x, y):
        all_indices = list(train_index) + list(test_index)
        n_test = len(list(test_index))
        break
    test_idxs = all_indices[-n_test:]
    valid_idxs = all_indices[-2*n_test:-n_test]
    train_idxs = all_indices[:-2*n_test]
    sum_flags = []
    for idxs in [train_idxs, valid_idxs, test_idxs]:
        sum_flags.append(y.iloc[idxs].sum()[0])

    x_train = x.iloc[train_idxs,:]    
    x_valid = x.iloc[valid_idxs,:]    
    x_test  = x.iloc[test_idxs,:]
    x_train.to_csv(FILE_NAME.replace(ext, "_X_train.csv"), index=False)
    x_valid.to_csv(FILE_NAME.replace(ext, "_X_valid.csv"), index=False)
    x_test.to_csv(FILE_NAME.replace(ext, "_X_test.csv"), index=False)


download_odds(".")





