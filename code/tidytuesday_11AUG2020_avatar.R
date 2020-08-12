# Load packages
library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(ggtext)
library(tvthemes)
library(extrafont)

# Load data
tuesdata <- tidytuesdayR::tt_load(2020, week = 33)
avatar <- tuesdata$avatar
data('stop_words')
afinn <- get_sentiments('afinn')

# Load font
import_avatar()
windowsFonts(sans="Slayer")
loadfonts(device="win")
loadfonts(device="postscript")

# Wrangle data
chrono <- avatar %>% 
  select(book_num, chapter_num) %>% 
  mutate(book_chapter=paste0(book_num,'-',chapter_num)) %>% 
  group_by(book_chapter,chapter_num, book_num) %>% 
  summarize() %>% 
  arrange(book_num, chapter_num) %>% 
  rowid_to_column('ep')

words <- avatar[,c(1,3,5,6,8)] %>% 
  filter(character=='Aang') %>% 
  unnest_tokens(word, character_words) %>% 
  anti_join(stop_words) %>% 
  left_join(afinn) %>% 
  left_join(chrono[,c(1,3,4)]) %>% 
  mutate(value=replace_na(value,0)) %>% 
  group_by(ep) %>% 
  summarize(aangst=sum(value))

# Create plot
fig <- ggplot() +
  geom_rect(aes(xmin=-1, xmax=20.5, ymin=-Inf, ymax=Inf),
            fill='#cfe4f5', color='#cfe4f5') +
  geom_rect(aes(xmin=20.5, xmax=40.5, ymin=-Inf, ymax=Inf),
            fill='#cdd7b3', color='#cdd7b3') +
  geom_rect(aes(xmin=40.5, xmax=62, ymin=-Inf, ymax=Inf),
            fill='#f9e0ce', color='#f9e0ce') +
  geom_hline(yintercept=0, color='grey20', size=2) +
  geom_point(data=words, aes(x=ep, y=aangst, color=aangst),
             size=3) +
  geom_smooth(data=words, aes(x=ep, y=aangst, color=..y..),
              method='lm', size=3, fullrange=TRUE) +
  scale_color_gradient2(low='red', mid='grey30', high='blue',
                        midpoint=0) +
  scale_x_continuous(expand=c(0,0), limits=c(-1,62)) +
  scale_y_continuous(limits = c(-40,30), breaks=c(-28,0,17)) +
  theme_void() +
  theme(plot.background = element_rect(fill = 'grey60'),
        axis.title.y = element_text(angle=90, family='Slayer',
                                    size=16, color='grey20'),
        legend.key.height = unit(0.7, 'in'),
        legend.text = element_text(size=12),
        legend.title = element_blank(),
        plot.margin = unit(c(0,0.2,0,0.2), 'in')) +
  labs(y='emotional tone\n\n') +
  annotate('text', x=30.5, y=28,
           label='A A N G S T :',
           size=10, family='Slayer', fontface='bold',
           color='grey20') +
  annotate('text', x=30.5, y=22,
           label='an exploration of aangs emotions through time',
           size=4.9, family='Slayer',
           color='grey20') +
  annotate('text', x=c(10,30.5,51), y=-27,
           label=c('book one:','book two:','book three:'),
           color=c('#6389a6','#859c51','#e87731'),
           size=5, family='Slayer') +
  annotate('text', x=c(10,30.5,51), y=-31,
           label=c('water','earth','fire'),
           color=c('#6389a6','#859c51','#e87731'),
           size=7.5, family='Slayer', fontface='bold') +
  annotate('text', x=-0.5, y=c(-27,0,16),
           label=c('negative\n\n','neutral\n\n','positive\n\n'),
           color=c('red','grey20','blue'),
           size=4, family='Slayer', fontface='bold', angle=90) +
  annotate('text', x=30.5, y=-39,
           label='each point represents the average sentiment score of aangs dialogue\nper chapter, fitted with a linear model. valence ratings\nare from the AFINN sentiment lexicon.',
           color='grey20', size=3.5, family='Slayer', fontface='italic') +
  coord_cartesian(clip='off')

ggsave('figs/aangst.png', fig, width=9, height=6, units='in')
