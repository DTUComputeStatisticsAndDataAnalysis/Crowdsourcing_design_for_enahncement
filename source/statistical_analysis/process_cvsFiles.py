#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct 14 10:44:16 2022

@author: sned
"""
import pandas as pd
from matplotlib import pyplot as plt
from itertools import combinations
import numpy as np

import statsmodels.api as sm
import seaborn as sns

def get_coef_table(lin_reg, avg_idx, sample_size, obj_metric, modality, dict_val):
    ''' lin_reg is a fitted statsmodels regression model
    Return a dataframe containing coefficients, pvalues, and the confidence intervals
    '''

    dict_val['Coeff'].append(lin_reg.params[1].round(4))
    dict_val['p-val'].append(lin_reg.pvalues[1].round(4))
    dict_val['Sample-size'].append(sample_size)
    dict_val['Avg-idx'].append(avg_idx)
    dict_val['Std-error'].append(lin_reg.bse[1].round(4))
    dict_val['Adj-R2'].append(lin_reg.rsquared_adj)
    dict_val['Objective-metric'].append(obj_metric)
    dict_val['Modality'].append(modality)


    return dict_val

if 0:
    file_path1 = '/df_clips_all_gold.csv'
    data_input = pd.read_csv(file_path1)
    gender_list = []
    clip_list = []
    for sample_idx in range(len(data_input)):
        clip_name_tmp = data_input.iloc[sample_idx]['short_file_name']
        if int(clip_name_tmp[2])<5:
            gender_list.append('M')
        else:
            gender_list.append('F')

        clip_list.append(int(clip_name_tmp[2]))

    data_input['Gender'] = gender_list
    data_input['ClipID'] = clip_list


    data_input.to_csv('/df_clips_all_processed_gold.csv')

if 0:
    #####Clip sizes
    file_path1 = '/df_clips_all_processed_gold.csv'
    data_input = pd.read_csv(file_path1)

    avg_dict = {'Objective-metric':[], 'Condition':[], 'Modality':[], 'Avg-idx':[], 'Sample-size':[],
                'CMOS':[], 'Obj':[], 'Alg':[]}
    sample_avg_list = [10, 8, 6, 4, 2]
    obj_list = ['dnsmos_SIG', 'dnsmos_BAK', 'dnsmos_OVR', 'visqol_MOS', '3quest_S', '3quest_N', '3quest_G']

    alg_list = [1]
    for sample_avg in sample_avg_list:
    # for sample_avg in [8]:
        comb_M = list(combinations([0, 1, 2, 3, 4], int(sample_avg/2)))
        comb_F = list(combinations([5, 6, 7, 8, 9], int(sample_avg/2)))

        for obj_val in obj_list:

            for type_idx in data_input['mean_positive'].unique():
                type_subset = data_input[data_input['mean_positive']==type_idx]
                if type_idx == 1:
                    type_val = 'Pos'
                elif type_idx == 0:
                    type_val = 'Neg'
                else:
                    print('error')
                count = 1
                for comb_M_idx in comb_M:
                    for comb_F_idx in comb_F:
                        print(comb_M_idx, comb_F_idx)
                        comb_clips = comb_M_idx + comb_F_idx


                        for condition in data_input['condition_num'].unique():
                            cmos_list = []
                            obj_num = []
                            tmp = type_subset[type_subset['condition_num']==condition]
                            # tmp_subset = tmp[tmp['condition_alg']==alg_list[0]]

                            for alg_idx in tmp['condition_alg'].unique():
                                tmp_subset = tmp[tmp['condition_alg']==alg_idx]
                                for clip_idx in comb_clips:
                                    tmp_2 = tmp_subset[tmp_subset['ClipID']==clip_idx]
                                    cmos_list.append(float(tmp_2['CMOS']))
                                    obj_num.append(float(tmp_2[obj_val]))

                                avg_dict['Objective-metric'].append(obj_val)
                                avg_dict['Condition'].append(condition)
                                avg_dict['Modality'].append(type_val)
                                avg_dict['Avg-idx'].append(count)
                                avg_dict['Sample-size'].append(sample_avg)
                                avg_dict['CMOS'].append(np.mean(cmos_list))
                                avg_dict['Obj'].append(np.mean(obj_num))
                                avg_dict['Alg'].append(alg_idx)

                        count += 1

    avg_df = pd.DataFrame(avg_dict)
    avg_df.to_csv('/avg_vals_size.csv')

if 1:


    avg_df_orig00 = pd.read_csv('avg_vals_size.csv')
    lm_dict = {'Objective-metric':[], 'Modality':[], 'Avg-idx':[], 'Sample-size':[],
                'Coeff':[], 'Std-error':[], 'p-val':[], 'Adj-R2':[]}
    obj_list_SF = ['DNS-S', 'DNS-B', 'DNS-O', 'ViS', '3Q-S', '3Q-N', '3Q-G']
    obj_list = ['dnsmos_SIG', 'dnsmos_BAK', 'dnsmos_OVR', 'visqol_MOS', '3quest_S', '3quest_N', '3quest_G']

    for type_idx in avg_df_orig00['Modality'].unique():
        avg_df_orig0 = avg_df_orig00[avg_df_orig00['Modality']==type_idx]
        for obj_idx in avg_df_orig0['Objective-metric'].unique():
            avg_df_orig = avg_df_orig0[avg_df_orig0['Objective-metric']==obj_idx]
            for sample_avg in avg_df_orig['Sample-size'].unique():
                avg_df = avg_df_orig[avg_df_orig['Sample-size']==sample_avg]
                for avg_idx in avg_df['Avg-idx'].unique():
                    print(sample_avg, avg_idx)
                    tmp_subset = avg_df[(avg_df['Sample-size']==sample_avg)&(avg_df['Avg-idx']==avg_idx)]
                    x = tmp_subset['CMOS'].values
                    x = sm.add_constant(x, prepend=True)
                    y = tmp_subset['Obj'].values
                    lm = sm.OLS(y,x).fit() # fitting the model
                    # lm.summary()
                    obj_idx_sf = obj_list_SF[obj_list.index(obj_idx)]
                    lm_dict = get_coef_table(lm, avg_idx, sample_avg, obj_idx_sf, type_idx, lm_dict)
                    print(sample_avg, avg_idx)
    lm_df = pd.DataFrame(lm_dict)
    plt.style.use('plot_style_orig.txt')
    plt.rcParams.update({'font.size': 20})
    flare_colors = sns.color_palette("flare")
    blue_colors = sns.color_palette("ch:s=.25,rot=-.25")
    red_colors = sns.color_palette("coolwarm")

    palette_use = [flare_colors[0], flare_colors[2], flare_colors[4],'#045346',blue_colors[0], blue_colors[2], blue_colors[4]]
    marker_use = ['*', '*', '*', 'o', 's', 's', 's']
    line_use = ['-.', '-.', '-.', '--', ':', ':', ':']

    plt.figure(figsize=(8, 4))
    sns.pointplot(data=lm_df[lm_df['Modality']=='Pos'], x="Sample-size", y="Coeff",
                  hue='Objective-metric', dodge=True, capsize=0.075, palette=palette_use, markers=marker_use, errwidth=1,
                  linestyles=line_use, scale=0.7, errorbar="sd")
    plt.title('CMOS-Coefficients (Pos)')
    plt.legend(ncol=2)
    plt.grid()
    plt.savefig('/pos_coeff.pdf', bbox_inches='tight')

    # plt.figure()
    # sns.pointplot(data=lm_df[lm_df['Modality']=='Pos'], x="Sample-size", y="Std-error",
    #               hue='Objective-metric', dodge=True, capsize=0.075, palette=palette_use, markers=marker_use, errwidth=1,
    #               linestyles=line_use, scale=0.7, errorbar="sd")
    plt.figure(figsize=(8, 4))
    sns.pointplot(data=lm_df[lm_df['Modality']=='Pos'], x="Sample-size", y="Adj-R2",
                  hue='Objective-metric', dodge=True, capsize=0.075, palette=palette_use, markers=marker_use, errwidth=1,
                  linestyles=line_use, scale=0.7, errorbar="sd")
    # plt.legend(ncol=2)
    plt.legend([])
    plt.title('Adjusted R2 (Pos)')
    plt.grid()
    plt.savefig('/pos_r2.pdf', bbox_inches='tight')


    plt.figure(figsize=(8, 4))
    sns.pointplot(data=lm_df[lm_df['Modality']=='Neg'], x="Sample-size", y="Coeff",
                  hue='Objective-metric', dodge=True, capsize=0.075, palette=palette_use, markers=marker_use, errwidth=1,
                  linestyles=line_use, scale=0.7, errorbar="sd")
    # plt.legend(ncol=2)
    plt.legend([])
    plt.title('CMOS-Coefficients (Neg)')
    plt.grid()
    plt.savefig('/neg_coeff.pdf', bbox_inches='tight')

    # plt.figure()
    # sns.pointplot(data=lm_df[lm_df['Modality']=='Neg'], x="Sample-size", y="Std-error",
    #               hue='Objective-metric', dodge=True, capsize=0.075, palette=palette_use, markers=marker_use, errwidth=1,
    #               linestyles=line_use, scale=0.7, errorbar="sd")
    plt.figure(figsize=(8, 4))
    sns.pointplot(data=lm_df[lm_df['Modality']=='Neg'], x="Sample-size", y="Adj-R2",
                  hue='Objective-metric', dodge=True, capsize=0.075, palette=palette_use, markers=marker_use, errwidth=1,
                  linestyles=line_use, scale=0.7, errorbar="sd")
    # plt.legend(ncol=2)
    plt.legend([])
    plt.title('Adjusted R2 (Neg)')
    plt.grid()
    plt.savefig('/neg_r2.pdf', bbox_inches='tight')

if 0:
    #####Clip sizes
    file_path1 = 'df_conditions_pos_neg_gold.csv'
    data_input = pd.read_csv(file_path1)


    sns.lmplot(data=data_input, x="CMOS", y="3quest_G",
        hue="mean_positive")
