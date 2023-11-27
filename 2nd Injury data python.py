# NFL 1st and Future 2019
## Can you investigate the relationship between the playing surface and the injury and performance of NFL athletes?

![](https://1ycbx02rgnsa1i87hd1i7v1r-wpengine.netdna-ssl.com/wp-content/uploads/2019/01/nfl.png)

This kernel is made in hopes of helping those interested in joining the competition get a jump start on the data. Much of the text was taken directly from the competition description. However be sure to read the official rules and data description on the kaggle website [here](https://www.kaggle.com/c/nfl-playing-surface-analytics).

tl;dr:
  
  **In this challenge, you're tasked to investigate the relationship between the playing surface and the injury and performance of National Football League (NFL) athletes and to examine factors that may contribute to lower extremity injuries.**

Submissions will be judged by the NFL based on how well they address:
- Representation of player movement, including, but not limited to, the development of novel metrics that characterize player movement on the field:
- Identification of specific variables that present an elevated risk of injury:
- Evaluation of differences in player movement between playing surfaces:

Submissions will be scored using the following rubric:
- Creativity and Presentation (5 points)
- Methodology (5 points)
- Application (5 points)

import numpy as np
import pandas as pd
import matplotlib.pylab as plt
import seaborn as sns
import matplotlib.patches as patches
sns.set_style("whitegrid")

# Data:
There are three files provided in the dataset, as described below:

1. **Injury Record:** The injury record file in .csv format contains information on 105 lower-limb injuries that occurred during regular season games over the two seasons. Injuries can be linked to specific records in a player history using the PlayerKey, GameID, and PlayKey fields.

2. **Play List:** – The play list file contains the details for the 267,005 player-plays that make up the dataset. Each play is indexed by PlayerKey, GameID, and PlayKey fields. Details about the game and play include the player’s assigned roster position, stadium type, field type, weather, play type, position for the play, and position group.

3. **Player Track Data:** player level data that describes the location, orientation, speed, and direction of each player during a play recorded at 10 Hz (i.e. 10 observations recorded per second).


# Read the input files
playlist = pd.read_csv('../input/nfl-playing-surface-analytics/PlayList.csv')
inj = pd.read_csv('../input/nfl-playing-surface-analytics/InjuryRecord.csv')
trk = pd.read_csv('../input/nfl-playing-surface-analytics/PlayerTrackData.csv')

## Injury Data
First lets look at the injury data. It's a fairly small file with only 105 injury plays shown. I notice that many of the rows for the injury plays do not show `PlayerKey`, `GameId`, etc. I'm not sure if this is a bug or intentially done.

- PlayerKey, GameId, PlayKey
- BodyPart
- Surface
- DM_M1, DM_M7, DM_28, DM_42 - One hot encoding the number of days missed for injury


# 28 Injurties without PlayKey
inj['PlayKey'].isna().sum()

inj.groupby('BodyPart').count()['PlayerKey'] \
    .sort_values() \
    .plot(kind='bar', figsize=(15, 5), title='Count of injuries by Body Part')
plt.show()

inj.groupby('Surface').count()['PlayerKey'] \
    .sort_values() \
    .plot(kind='barh', figsize=(15, 5), title='Count of injuries by Field Surface', color='orange')
plt.show()

inj.groupby(['BodyPart','Surface']) \
    .count() \
    .unstack('BodyPart')['PlayerKey'] \
    .T.sort_values('Natural').T \
    .sort_values('Ankle') \
    .plot(kind='bar', figsize=(15, 5), title='Injury Body Part by Turf Type')
plt.show()

## Playlist Data

# Number of unique plays in the playlist dataset
playlist['PlayKey'].nunique()

playlist[['PlayKey','PlayType']].drop_duplicates() \
    .groupby('PlayType').count()['PlayKey'] \
    .sort_values() \
    .plot(kind='barh',
         figsize=(15, 6),
          color='black',
         title='Number of plays provided by type')
plt.show()

## Match Player info with injury data
- Only 77 link up the player info

inj_detailed = inj.merge(playlist)

inj_detailed.groupby('RosterPosition').count()['PlayerKey'] \
    .sort_values() \
    .plot(figsize=(15, 5), kind='barh', title='Injured Players by Position')
plt.show()

inj_detailed.groupby('PlayType').count()['PlayerKey'] \
    .sort_values() \
    .plot(figsize=(15, 5), kind='barh', title='Injured Players by PlayType', color='green')
plt.show()

## Distribution of Injury Types

inj_detailed.groupby(['RosterPosition','BodyPart']) \
    .count() \
    .unstack('BodyPart')['PlayerKey'] \
    .T.apply(lambda x: x / x.sum()) \
    .sort_values('BodyPart').T.sort_values('Ankle', ascending=False) \
    .plot(kind='barh',
          figsize=(15, 5),
          title='Injury Body Part by Player Position',
          stacked=True)
plt.show()

inj_detailed.groupby(['PlayType','BodyPart']) \
    .count() \
    .unstack('BodyPart')['PlayerKey'] \
    .T.apply(lambda x: x / x.sum()) \
    .sort_values('BodyPart').T.sort_values('Ankle', ascending=False) \
    .plot(kind='barh',
          figsize=(15, 5),
          title='Injury Body Part by Play Type',
          stacked=True)
plt.show()

inj_detailed.groupby(['RosterPosition','Surface']) \
    .count() \
    .unstack('Surface')['PlayerKey'] \
    .T.apply(lambda x: x / x.sum()) \
    .sort_values('Surface').T.sort_values('Natural', ascending=False) \
    .plot(kind='barh',
          figsize=(15, 5),
          title='Injury Body Part by Turf Type',
          stacked=True)
plt.show()

# Plotting Plays
Check out my kernel here where I provide a function for creating and plotting a NFL football field.

https://www.kaggle.com/robikscube/nfl-big-data-bowl-plotting-player-position


def create_football_field(linenumbers=True,
                          endzones=True,
                          highlight_line=False,
                          highlight_line_number=50,
                          highlighted_name='Line of Scrimmage',
                          fifty_is_los=False,
                          figsize=(12, 6.33)):
    """
    Function that plots the football field for viewing plays.
    Allows for showing or hiding endzones.
    """
    rect = patches.Rectangle((0, 0), 120, 53.3, linewidth=0.1,
                             edgecolor='r', facecolor='darkgreen', zorder=0)

    fig, ax = plt.subplots(1, figsize=figsize)
    ax.add_patch(rect)

    plt.plot([10, 10, 10, 20, 20, 30, 30, 40, 40, 50, 50, 60, 60, 70, 70, 80,
              80, 90, 90, 100, 100, 110, 110, 120, 0, 0, 120, 120],
             [0, 0, 53.3, 53.3, 0, 0, 53.3, 53.3, 0, 0, 53.3, 53.3, 0, 0, 53.3,
              53.3, 0, 0, 53.3, 53.3, 0, 0, 53.3, 53.3, 53.3, 0, 0, 53.3],
             color='white')
    if fifty_is_los:
        plt.plot([60, 60], [0, 53.3], color='gold')
        plt.text(62, 50, '<- Player Yardline at Snap', color='gold')
    # Endzones
    if endzones:
        ez1 = patches.Rectangle((0, 0), 10, 53.3,
                                linewidth=0.1,
                                edgecolor='r',
                                facecolor='blue',
                                alpha=0.2,
                                zorder=0)
        ez2 = patches.Rectangle((110, 0), 120, 53.3,
                                linewidth=0.1,
                                edgecolor='r',
                                facecolor='blue',
                                alpha=0.2,
                                zorder=0)
        ax.add_patch(ez1)
        ax.add_patch(ez2)
    plt.xlim(0, 120)
    plt.ylim(-5, 58.3)
    plt.axis('off')
    if linenumbers:
        for x in range(20, 110, 10):
            numb = x
            if x > 50:
                numb = 120 - x
            plt.text(x, 5, str(numb - 10),
                     horizontalalignment='center',
                     fontsize=20,  # fontname='Arial',
                     color='white')
            plt.text(x - 0.95, 53.3 - 5, str(numb - 10),
                     horizontalalignment='center',
                     fontsize=20,  # fontname='Arial',
                     color='white', rotation=180)
    if endzones:
        hash_range = range(11, 110)
    else:
        hash_range = range(1, 120)

    for x in hash_range:
        ax.plot([x, x], [0.4, 0.7], color='white')
        ax.plot([x, x], [53.0, 52.5], color='white')
        ax.plot([x, x], [22.91, 23.57], color='white')
        ax.plot([x, x], [29.73, 30.39], color='white')

    if highlight_line:
        hl = highlight_line_number + 10
        plt.plot([hl, hl], [0, 53.3], color='yellow')
        plt.text(hl + 2, 50, '<- {}'.format(highlighted_name),
                 color='yellow')
    return fig, ax


## Plot path of injured player

example_play_id = inj['PlayKey'].values[0]
fig, ax = create_football_field()
trk.query('PlayKey == @example_play_id').plot(kind='scatter', x='x', y='y', ax=ax, color='orange')
plt.show()

## Plotting every route of injured players
- Too much info to draw conclusions, but fun to plot for context.

# Loop through all 99 inj plays
inj_play_list = inj['PlayKey'].tolist()
fig, ax = create_football_field()
for playkey, inj_play in trk.query('PlayKey in @inj_play_list').groupby('PlayKey'):
    inj_play.plot(kind='scatter', x='x', y='y', ax=ax, color='orange', alpha=0.2)
plt.show()

## Plotting routes of some non-injured players

import random
playids = trk['PlayKey'].unique() #.sample(100)
non_inj_play = [x for x in playids if x not in inj_play_list]
sample_non_inj_plays = random.sample(non_inj_play, 100)

fig, ax = create_football_field()
for playkey, inj_play in trk.query('PlayKey in @sample_non_inj_plays').groupby('PlayKey'):
    inj_play.plot(kind='scatter', x='x', y='y', ax=ax, color='red', alpha=0.2)
plt.show()

## Distribution of tracking info for players with injuries
fig, axes = plt.subplots(1, 2)

trk.query('PlayKey in @inj_play_list')['s'].plot(kind='hist',
                                                 title='Distribution of player Speed injured',
                                                 figsize=(15, 5), bins=30, ax=axes[0])
trk.query('PlayKey not in @inj_play_list')['s'].sample(10000).plot(kind='hist',
                                                 title='Distribution of player Speed not injured',
                                                 figsize=(15, 5), bins=30, ax=axes[1], color='orange')
plt.show()

fig, axes = plt.subplots(1, 2)

trk.query('PlayKey in @inj_play_list')['o'].plot(kind='hist',
                                                 title='Distribution of player Orientation injured',
                                                 figsize=(15, 5), bins=30, ax=axes[0])
trk.query('PlayKey not in @inj_play_list')['o'].sample(10000).plot(kind='hist',
                                                 title='Distribution of player Orientation not injured',
                                                 figsize=(15, 5), bins=30, ax=axes[1], color='orange')
plt.show()

### Differences in x, y could be attributed to the player positions which are more likely to have injury.

fig, axes = plt.subplots(1, 2)

trk.query('PlayKey in @inj_play_list')['x'].plot(kind='hist',
                                                 title='Distribution of player X injured',
                                                 figsize=(15, 5), bins=30, ax=axes[0])
trk.query('PlayKey not in @inj_play_list')['x'].sample(10000).plot(kind='hist',
                                                 title='Distribution of player X not injured',
                                                 figsize=(15, 5), bins=30, ax=axes[1], color='orange')
plt.show()

fig, axes = plt.subplots(1, 2)

trk.query('PlayKey in @inj_play_list')['y'].plot(kind='hist',
                                                 title='Distribution of player Y injured',
                                                 figsize=(15, 5), bins=30, ax=axes[0])
trk.query('PlayKey not in @inj_play_list')['y'].sample(10000).plot(kind='hist',
                                                 title='Distribution of player Y not injured',
                                                 figsize=(15, 5), bins=30, ax=axes[1], color='orange')
plt.show()

# Compass plots of direction/velocity

def compass(angles, radii, arrowprops=None, ax=None):
    """
    * Modified for NFL data plotting
    Compass draws a graph that displays the vectors with
    components `u` and `v` as arrows from the origin.

    Examples
    --------
    >>> import numpy as np
    >>> u = [+0, +0.5, -0.50, -0.90]
    >>> v = [+1, +0.5, -0.45, +0.85]
    >>> compass(u, v)
    """

    #angles, radii = cart2pol(u, v)
    if ax is None:
        fig, ax = plt.subplots(subplot_kw=dict(polar=True))

    kw = dict(arrowstyle="->", color='k')
    if arrowprops:
        kw.update(arrowprops)
    [ax.annotate("", xy=(angle, radius), xytext=(0, 0),
                 arrowprops=kw) for
     angle, radius in zip(angles, radii)]

    ax.set_ylim(0, np.max(radii))

    return ax

def plot_play_compass(playkey, **kwargs):
    d = trk.loc[trk['PlayKey'] == playkey].copy()
    d['dir_theta'] = d['dir'] * np.pi / 180
    # Calculate velocity in meters per second
    d['dis_meters'] = d['dis'] / 1.0936  # Add distance in meters
    # Speed
    d['dis_meters'] / 0.01
    d['v_mps'] = d['dis_meters'] / 0.1

    ax = compass(d['dir_theta'], d['v_mps'],
                  arrowprops={'alpha': 0.3},
                **kwargs)
    return ax
-------------------------------------------------------------------------------------------------
fig, axes = plt.subplots(2, 3,  subplot_kw=dict(polar=True), figsize=(15, 10))
axes = np.array(axes)
axes = axes.reshape(-1)

i = 0
for p in inj_detailed['PlayKey'].values[:6]:
    plot_play_compass(p, ax=axes[i])
    axes[i].set_title(f'PlayKey: {p}')
    i += 1
plt.show()

# Player Position vs Compass Plot

# Play Details
example_play_id = inj['PlayKey'].values[6]
inj_detailed.query('PlayKey == @example_play_id')

fig, ax = create_football_field(ax)
ax.set_title(f'PlayKey: {example_play_id}')
trk.query('PlayKey == @example_play_id').plot(kind='scatter', x='x', y='y', ax=ax, color='orange')
plt.show()

ax = plot_play_compass(example_play_id)
ax.set_title(f'PlayKey: {p}')
plt.show()

# Plays by max speed

trk.groupby('PlayKey')[['s']].max() \
    .sort_values('s', ascending=False) \
    .query('s != 0').head(20) \
    .plot(kind='barh', figsize=(15, 5), title='Top 20 Plays by Max Player Speed')
plt.show()

trk.groupby('PlayKey')[['s']].max() \
    .sort_values('s', ascending=True) \
    .query('s != 0').head(20) \
    .plot(kind='barh', figsize=(15, 5), title='Bottom 20 Plays by Min Player Speed')
plt.show()

## Injury Length
- DM columns tell us a one-hot encoding for the number of days missed due to injury
- 1+, 7+, 28+, and 48+ days missed

We can see that all the injuries caused at least 1 day missing and around 30% missed 48+ days.

inj[['DM_M1','DM_M7','DM_M28','DM_M42']].mean() \
    .plot(figsize=(15, 5),
          kind='bar',
          title='Percent of injuries by injury length')
plt.show()

# Look at the top of each data file

inj.head()
trk.head()
playlist.head()


