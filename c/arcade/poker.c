#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <time.h>
#include <assert.h>

#define POKER_MIN_PLAYERS	2
#define POKER_MAX_PLAYERS	9

enum rank
{
	R2,
	R3,
	R4,
	R5,
	R6,
	R7,
	R8,
	R9,
	R10,
	JACK,
	QUEEN,
	KING,
	ACE,
	RANK_SIZE
};


enum suit
{
	CLUBS,
	HEARTS,
	DIAMONDS,
	SPADES,
	SUIT_SIZE
};


enum hand_tag
{
	HIGH,
	PAIR_1,
	PAIR_2,
	KIND_3,
	STRAIGHT,
	FLUSH,
	F_HOUSE,
	KIND_4,
	S_FLUSH
};


typedef enum rank rank_t;
typedef enum suit suit_t;
typedef enum hand_tag hand_tag_t;


struct card;
struct hand;
struct hand_set;
struct table;


struct card
{
	rank_t rank;
	rank_t suit;
};


struct hand
{
	hand_tag_t tag;
	union {
		rank_t s_flush;
		rank_t kind_4[2];
		rank_t f_house[2];
		rank_t flush[5];
		rank_t straight;
		rank_t kind_3[3];
		rank_t pair_2[3];
		rank_t pair_1[4];
		rank_t high[5];
	} data;
};


struct table
{
	struct card flop[3];
	struct card turn;
	struct card river;
};


typedef struct card card_t;
typedef struct hand hand_t;
typedef struct card p_hand_t[2];
typedef struct card hand_set_t[5];
typedef struct table table_t;

int poker_compare_hand(const hand_t *, const hand_t *);

/* the number of players must be between 2-9 */
void poker_deal(int num_players, table_t *, p_hand_t *);

void poker_best_hand(const table_t *, const p_hand_t *,
	hand_t *, hand_set_t *);


static card_t deck(int idx)
{
	assert(idx >= 0);
	assert(idx < RANK_SIZE * SUIT_SIZE);
	return (card_t) {
		.rank = idx % RANK_SIZE,
		.suit = idx / RANK_SIZE
	};
}


static const char *rank_str(rank_t r)
{
	switch (r) {
	case R2:
		return "2";
	case R3:
		return "3";
	case R4:
		return "4";
	case R5:
		return "5";
	case R6:
		return "6";
	case R7:
		return "7";
	case R8:
		return "8";
	case R9:
		return "9";
	case R10:
		return "10";
	case JACK:
		return "J";
	case QUEEN:
		return "Q";
	case KING:
		return "K";
	case ACE:
		return "A";
	default:
		assert(false);
		break;
	}
	return '\0';
}


static char suit_char(suit_t s)
{
	switch (s) {
	case CLUBS:
		return 'c';
	case HEARTS:
		return 'h';
	case DIAMONDS:
		return 'd';
	case SPADES:
		return 's';
	default:
		assert(false);
		break;
	}
	return '\0';
}


static int snprint_card(card_t card, size_t n, char *dest)
{
	return snprintf(dest, n, "%s%c",
		rank_str(card.rank), suit_char(card.suit));
}


static void print_card(card_t card)
{
	char buf[4];
	snprint_card(card, 4, buf);
	printf("%s", buf);
}


static void print_p_hand(p_hand_t *p_hand)
{
	const int buf_size = 256;
	char buf[buf_size];
	int ofs = 0;

	ofs += snprintf(buf + ofs, buf_size - ofs, "{");
	for (int i = 0; i < 2; i ++) {
		ofs += snprint_card((*p_hand)[i], buf_size - ofs, buf + ofs);
		if (i < 2 - 1)
			ofs += snprintf(buf + ofs, buf_size - ofs, ", ");
	}
	ofs += snprintf(buf + ofs, buf_size - ofs, "}");
	puts(buf);
}


static void print_table(table_t *table)
{
	const int buf_size = 256;
	char buf[buf_size];
	int ofs = 0;

	ofs += snprintf(buf + ofs, buf_size - ofs, "table { .flop = { ");
	for (int i = 0; i < 3; i ++) {
		ofs += snprint_card(table->flop[i], buf_size - ofs, buf + ofs);
		if (i < 3 - 1)
			ofs += snprintf(buf + ofs, buf_size - ofs, ", ");
	}

	ofs += snprintf(buf + ofs, buf_size - ofs, " }, .turn = ");
	ofs += snprint_card(table->turn, buf_size - ofs, buf + ofs);

	ofs += snprintf(buf + ofs, buf_size - ofs, ", .river = ");
	ofs += snprint_card(table->river, buf_size - ofs, buf + ofs);

	ofs += snprintf(buf + ofs, buf_size - ofs, " }");
	puts(buf);
}


static void print_hand_tag(hand_tag_t tag)
{
	switch (tag) {
	case HIGH:
		printf("HIGH");
		break;
	case PAIR_1:
		printf("PAIR_1");
		break;
	case PAIR_2:
		printf("PAIR_2");
		break;
	case KIND_3:
		printf("KIND_3");
		break;
	case STRAIGHT:
		printf("STRAIGHT");
		break;
	case FLUSH:
		printf("FLUSH");
		break;
	case F_HOUSE:
		printf("F_HOUSE");
		break;
	case KIND_4:
		printf("KIND_4");
		break;
	case S_FLUSH:
		printf("S_FLUSH");
		break;
	default:
		assert(false);
		break;
	}
}


static void print_hand_set(hand_set_t *hand_set)
{
	for (int i = 0; i < 5; i++) {
		print_card((*hand_set)[i]);
		putchar(' ');
	}
}


static int compare_rank(rank_t r1, rank_t r2)
{
	if (r1 > r2)
		return 1;
	if (r1 < r2)
		return -1;
	return 0; /* r1 = r2 */
}


static int compare_rank_n(int n, const rank_t *r1, const rank_t *r2)
{
	for (int i = 0; i < n; i++) {
		int res = compare_rank(r1[i], r2[i]);
		if (res != 0)
			return res;
	}
	return 0;
}


static int compare_hand(const hand_t *h1, const hand_t *h2)
{
	if (h1->tag > h2->tag)
		return 1;
	if (h1->tag < h2->tag)
		return -1;
	switch (h1->tag) { /* h1-tag = h2-tag */
	case S_FLUSH:
		return compare_rank(h1->data.s_flush, h2->data.s_flush);
	case KIND_4:
		return compare_rank_n(2, h1->data.kind_4, h2->data.kind_4);
	case F_HOUSE:
		return compare_rank_n(2, h1->data.f_house, h2->data.f_house);
	case FLUSH:
		return compare_rank_n(5, h1->data.flush, h2->data.flush);
	case STRAIGHT:
		return compare_rank(h1->data.straight, h1->data.straight);
	case KIND_3:
		return compare_rank_n(3, h1->data.kind_3, h2->data.kind_3);
	case PAIR_2:
		return compare_rank_n(3, h1->data.pair_2, h2->data.pair_2);
	case PAIR_1:
		return compare_rank_n(4, h1->data.pair_1, h2->data.pair_1);
	case HIGH:
		return compare_rank_n(5, h1->data.high, h2->data.high);
	default:
		assert(false);
		break;
	}
	return 0;
}


int poker_compare_hand(const hand_t *h1, const hand_t *h2)
{
	return compare_hand(h1, h2);
}



static int deal_cmp_aux(const void *a, const void *b)
{
	return *(int*)a - *(int*)b;
}


void poker_deal(int num_players, table_t *table, p_hand_t *p_hand)
{
	assert(num_players >= POKER_MIN_PLAYERS);
	assert(num_players <= POKER_MAX_PLAYERS);
	
	const int deck_size = RANK_SIZE * SUIT_SIZE;

	/* Think of each elem containing two int's (rand_idx, deck_idx). */
	int rand_idx[deck_size * 2];
	for (int i = 0; i < deck_size; i++) {
		rand_idx[i * 2    ] = rand();
		rand_idx[i * 2 + 1] = i;
	}

	/* Shuffle deck indices by sorting their random indices */
	qsort(rand_idx, deck_size, sizeof(int) * 2, deal_cmp_aux);

	/* Only copy deck indices. */
	int idx[deck_size];
	for (int i = 0; i < deck_size; i++) {
		idx[i] = rand_idx[i * 2 + 1];
	}

	/* Deal table cards. */
	int j = 0;
	for (; j < 3; j++) {
		table->flop[j] = deck(idx[j]);
	}
	table->turn = deck(idx[j]);
	j++;
	table->river = deck(idx[j]);
	j++;

	/* Deal players' cards */
	for (int k = 0; k < num_players; k++, j++) {
		p_hand[k][0] = deck(idx[j    ]);
		p_hand[k][1] = deck(idx[j + 1]);
	}
}


static int descend_card_cmp_rank_aux(const void *c1, const void *c2)
{
	return compare_rank((*(card_t*)c2).rank, (*(card_t*)c1).rank);
}


static int descend_card_cmp_suit_aux(const void *c1, const void *c2)
{
	return compare_rank((*(card_t*)c2).suit, (*(card_t*)c1).suit);
}


/* Finds the lowest and the special-case straight of Ace to Five */
static bool best_hand_straight_low_n_aux(const card_t *cards, int n,
		hand_t *hand, hand_set_t *hand_set)
{
	int r5 = -1;
	int r4 = -1;
	int r3 = -1;
	int r2 = -1;
	int ace = -1;

	for (int i = 0; i < n; i++) {
		switch (cards[i].rank) {
		case R5:
			r5 = i;
			break;
		case R4:
			r4 = i;
			break;
		case R3:
			r3 = i;
			break;
		case R2:
			r2 = i;
			break;
		case ACE:
			ace = i;
			break;
		default:
			break;
		}
	}
	if (r5 == -1 || r4 == -1 || r3 == -1 || r2 == -1 || ace == -1)
		return false;

	hand->tag = STRAIGHT;
	hand->data.straight = R5;
	(*hand_set)[0] = cards[r5];
	(*hand_set)[1] = cards[r4];
	(*hand_set)[2] = cards[r3];
	(*hand_set)[3] = cards[r2];
	(*hand_set)[4] = cards[ace];
	return true;
}


/* Pre: Cards are sorted by descending rank and higher hands were attempted. */
static bool best_hand_straight_n_aux(const card_t *cards, int n,
		hand_t *hand, hand_set_t *hand_set)
{
	/* Find start index */
	int k = -1;
	for (int i = 0; i < n - 4; i++) {
		if (cards[i].rank == cards[i + 1].rank + 1) {
			k = i;
			break;
		}
	}
	if (k == -1)
		return best_hand_straight_low_n_aux(cards, n, hand, hand_set);

	/* Find five descending consecutive ranks. */
	int j = 0; /* Consecutive counter. */
	for (int i = k; i < n - 1 && j < 4; i++) {
		/* Is a consecutive rank? */
		if (cards[i].rank == cards[i + 1].rank + 1)
			j++;
		/* So it's not consecutive. But, if it's not equal, recount. */
		else if (cards[i].rank != cards[i + 1].rank)
			j = 0;
	}
	if (j < 4)
		return best_hand_straight_low_n_aux(cards, n, hand, hand_set);
	hand->tag = STRAIGHT;
	hand->data.straight = cards[k].rank;

	for (int i = k, m = 0; m < 4; i++) {
		assert(i < n - 1);
		if (cards[i].rank == cards[i + 1].rank + 1) {
			(*hand_set)[m] = cards[i];
			(*hand_set)[m + 1] = cards[i + 1];
			m++;
			continue;
		}
		if (cards[i].rank != cards[i + 1].rank) {
			m = 0;
			hand->data.straight = cards[i + 1].rank;
		}
	}
	return true;
}


/* Pre: Cards are sorted by descending rank and higher hands were attempted. */
static bool best_hand_s_flush_aux(const card_t c[7],
		hand_t *hand, hand_set_t *hand_set)
{
	card_t cards[7];
	memcpy(cards, c, sizeof(card_t) * 7);
	qsort(cards, 7, sizeof(card_t), descend_card_cmp_suit_aux);
	
	/* Find five to seven consecutive similar suits. */
	int j = -1;
	int k = 0;
	for (int i = 0; i < 7 - 4; i++) {
		if (cards[i].suit == cards[i + 1].suit &&
				cards[i].suit == cards[i + 2].suit &&
				cards[i].suit == cards[i + 3].suit &&
				cards[i].suit == cards[i + 4].suit) {
			j = i;
			if (i + 5 < 7 && cards[i].suit == c[i + 5].suit)
				k++;
			if (i + 6 < 7 && cards[i].suit == c[i + 6].suit)
				k++;
			break;
		}
	}
	/* Not found. Exit with failure. */
	if (j == -1)
		return false;
	qsort(cards + j, 5 + k, sizeof(card_t), descend_card_cmp_rank_aux);

	if (best_hand_straight_n_aux(cards + j, 5 + k, hand, hand_set)) {
		hand->tag = S_FLUSH;
		hand->data.s_flush = hand->data.straight;
		return true;
	}
	return false;
}


/* Pre: Cards are sorted by descending rank and higher hands were attempted. */
static bool best_hand_kind_4_aux(const card_t cards[7],
		hand_t *hand, hand_set_t *hand_set)
{
	/* Find two consecutive similar ranks. */
	int j = -1;
	for (int i = 0; i < 7 - 3; i++) {
		if (cards[i].rank == cards[i + 1].rank &&
				cards[i].rank == cards[i + 2].rank &&
				cards[i].rank == cards[i + 3].rank) {
			j = i;
			break;
		}
	}
	/* Not found. Exit with failure. */
	if (j == -1)
		return false;

	/* Copy pair. */	
	hand->tag = KIND_4; 
	hand->data.kind_4[0] = cards[j].rank;
	(*hand_set)[0] = cards[j];
	(*hand_set)[1] = cards[j + 1];
	(*hand_set)[2] = cards[j + 2];
	(*hand_set)[3] = cards[j + 3];

	/* Copy a kicker. */	
	for (int i = 0; i < 7; i++) {
		/* Check if it's a four of a kind. */
		if (i == j || i == j + 1 || i == j + 2 || i == j + 3)
			continue;
		(*hand_set)[4] = cards[i];
		hand->data.kind_4[1] = cards[i].rank;
		break;
	}
	return true;
}


/* Pre: Cards are sorted by descending rank and higher hands were attempted. */
static bool best_hand_f_house_aux(const card_t cards[7],
		hand_t *hand, hand_set_t *hand_set)
{
	/* Find a three consecutive similar ranks. */
	int j = -1;
	for (int i = 0; i < 7 - 1; i++) {
		if (cards[i].rank == cards[i + 1].rank &&
			cards[i].rank == cards[i + 2].rank) {
			j = i;
			break;
		}
	}
	/* No three of a kind found. Exit with failure. */
	if (j == -1)
		return false;

	/* Find a pair consecutive similar ranks. */
	int k = -1;
	for (int i = 0; i < 7 - 1; i++) {
		if (i != j && i != j + 1 && i == j + 2 &&
			cards[i].rank == cards[i + 1].rank &&
			cards[i].rank == cards[i + 2].rank) {
			k = i;
			break;
		}
	}
	/* No pair found. Exit with failure. */
	if (k == -1)
		return false;

	/* Copy two pairs. */	
	hand->tag = F_HOUSE; 
	hand->data.f_house[0] = cards[j].rank;
	hand->data.f_house[1] = cards[k].rank;
	(*hand_set)[0] = cards[j];
	(*hand_set)[1] = cards[j + 1];
	(*hand_set)[2] = cards[j + 2];
	(*hand_set)[3] = cards[k];
	(*hand_set)[4] = cards[k + 1];
	return true;
}


/* Pre: Cards are sorted by descending rank and higher hands were attempted. */
static bool best_hand_flush_aux(const card_t c[7],
		hand_t *hand, hand_set_t *hand_set)
{
	card_t cards[7];
	memcpy(cards, c, sizeof(card_t) * 7);
	qsort(cards, 7, sizeof(card_t), descend_card_cmp_suit_aux);
	
	/* Find five to seven consecutive similar suits. */
	int j = -1;
	int k = 0;
	for (int i = 0; i < 7 - 4; i++) {
		if (cards[i].suit == cards[i + 1].suit &&
				cards[i].suit == cards[i + 2].suit &&
				cards[i].suit == cards[i + 3].suit &&
				cards[i].suit == cards[i + 4].suit) {
			j = i;
			if (i + 5 < 7 && cards[i].suit == c[i + 5].suit)
				k++;
			if (i + 6 < 7 && cards[i].suit == c[i + 6].suit)
				k++;
			break;
		}
	}
	/* Not found. Exit with failure. */
	if (j == -1)
		return false;
	qsort(cards + j, 5 + k, sizeof(card_t), descend_card_cmp_rank_aux);

	hand->tag = FLUSH;
	for (int i = 0; i < 5; i++) {
		hand->data.flush[i] = cards[i + j].rank;
		(*hand_set)[i] = cards[i + j];
	}

	return true;
}


/* Pre: Cards are sorted by descending rank and higher hands were attempted. */
static bool best_hand_straight_aux(const card_t cards[7],
		hand_t *hand, hand_set_t *hand_set)
{
	return best_hand_straight_n_aux(cards, 7, hand, hand_set);
}


/* Pre: Cards are sorted by descending rank and higher hands were attempted. */
static bool best_hand_kind_3_aux(const card_t cards[7],
		hand_t *hand, hand_set_t *hand_set)
{
	/* Find two consecutive similar ranks. */
	int j = -1;
	for (int i = 0; i < 7 - 2; i++) {
		if (cards[i].rank == cards[i + 1].rank &&
				cards[i].rank == cards[i + 2].rank) {
			j = i;
			break;
		}
	}
	/* Not found. Exit with failure. */
	if (j == -1)
		return false;

	/* Copy pair. */	
	hand->tag = KIND_3; 
	hand->data.kind_3[0] = cards[j].rank;
	(*hand_set)[0] = cards[j];
	(*hand_set)[1] = cards[j + 1];
	(*hand_set)[2] = cards[j + 2];

	/* Copy 2 kickers. */	
	for (int i = 0, k = 0; i < 7 && k < 2; i++) {
		/* Check if is a pair index. */
		if (i == j || i == j + 1 || i == j + 2)
			continue;
		(*hand_set)[k + 3] = cards[i];
		hand->data.kind_3[k + 1] = cards[i].rank;
		k++;
	}
	return true;
}


/* Pre: Cards are sorted by descending rank and higher hands were attempted. */
static bool best_hand_pair_2_aux(const card_t cards[7],
		hand_t *hand, hand_set_t *hand_set)
{
	/* Find a pair of two consecutive similar ranks. */
	int j = -1;
	int k = -1;
	for (int i = 0; i < 7 - 1; i++) {
		if (cards[i].rank == cards[i + 1].rank) {
			if (j == -1) {
				j = i;
				continue;
			}
			k = i;
			break;
		}
	}
	/* Not found. Exit with failure. */
	if (j == -1 || k == -1)
		return false;

	/* Copy two pairs. */	
	hand->tag = PAIR_2; 
	hand->data.pair_2[0] = cards[j].rank;
	hand->data.pair_2[1] = cards[k].rank;
	(*hand_set)[0] = cards[j];
	(*hand_set)[1] = cards[j + 1];
	(*hand_set)[2] = cards[k];
	(*hand_set)[3] = cards[k + 1];

	/* Copy 1 kicker. */
	for (int i = 0; i < 7; i++) {
		/* step if in pair idx's */
		if (i == j || i == j + 1)
			continue;
		if (i == k || i == k + 1)
			continue;
		(*hand_set)[4] = cards[i];
		hand->data.pair_2[2] = cards[i].rank;
		break;
	}
	return true;
}


/* Pre: Cards are sorted by descending rank and higher hands were attempted. */
static bool best_hand_pair_1_aux(const card_t cards[7],
		hand_t *hand, hand_set_t *hand_set)
{
	/* Find two consecutive similar ranks. */
	int j = -1;
	for (int i = 0; i < 7 - 1; i++) {
		if (cards[i].rank == cards[i + 1].rank) {
			j = i;
			break;
		}
	}
	/* Not found. Exit with failure. */
	if (j == -1)
		return false;

	/* Copy pair. */	
	hand->tag = PAIR_1; 
	hand->data.pair_1[0] = cards[j].rank;
	(*hand_set)[0] = cards[j];
	(*hand_set)[1] = cards[j + 1];

	/* Copy 3 kickers. */	
	for (int i = 0, k = 0; i < 7 && k < 3; i++) {
		/* Check if is a pair index. */
		if (i == j || i == j + 1)
			continue;
		(*hand_set)[k + 2] = cards[i];
		hand->data.pair_1[k + 1] = cards[i].rank;
		k++;
	}
	return true;
}


/* Pre: Cards are sorted by descending rank and higher hands were attempted. */
static void best_hand_high_aux(const card_t cards[7],
		hand_t *hand, hand_set_t *hand_set)
{
	hand->tag = HIGH;
	for (int i = 0; i < 5; i++) {
		hand->data.high[i] = cards[i].rank;
		(*hand_set)[i] = cards[i];
	}
	(void)NULL;
}


void poker_best_hand(const table_t *table, const p_hand_t *p_hand,
		hand_t *hand, hand_set_t *hand_set)
{
	card_t cards[7] = {
		[0] = table->flop[0],
		[1] = table->flop[1],
		[2] = table->flop[2],
		[3] = table->turn,
		[4] = table->river,
		[5] = (*p_hand)[0],
		[6] = (*p_hand)[1],
	};
	/* Sort cards by rank in descending order. */
	qsort(cards, 7, sizeof(card_t), descend_card_cmp_rank_aux);

	/* Find a straight flush. */
	if (best_hand_s_flush_aux(cards, hand, hand_set))
		return;
	/* Find four of a kind. */
	if (best_hand_kind_4_aux(cards, hand, hand_set))
		return;
	/* Find a full house. */
	if (best_hand_f_house_aux(cards, hand, hand_set))
		return;
	/* Find a flush. */
	if (best_hand_flush_aux(cards, hand, hand_set))
		return;
	/* Find a straight. */
	if (best_hand_straight_aux(cards, hand, hand_set))
		return;
	/* Find three of a kind. */
	if (best_hand_kind_3_aux(cards, hand, hand_set))
		return;
	/* Find two pair. */
	if (best_hand_pair_2_aux(cards, hand, hand_set))
		return;
	/* Find one pair. */
	if (best_hand_pair_1_aux(cards, hand, hand_set))
		return;
	/* Resort to high card. */
	best_hand_high_aux(cards, hand, hand_set);
}


static float ai_weight(hand_tag_t tag)
{
	switch (tag) {
	case HIGH:
		return 0.01;
	case PAIR_1:
		return 0.01;
	case PAIR_2:
		return 0.01;
	case KIND_3:
		return 0.01;
	case STRAIGHT:
		return 0.25;
	case FLUSH:
		return 0.66;
	case F_HOUSE:
		return 0.77;
	case KIND_4:
		return 0.88;
	case S_FLUSH:
		return 0.99;
	default:
		assert(false);
		break;
	}
	return 0.0;
}

/*
static bool chance(float weight)
{
	if (weight >= 1.0)
		return true;
	if (weight <= 0.0)
		return false;
	return weight >= (float)rand() / RAND_MAX;
}

static void test_ai()
{
	const int N = 2;

	table_t table;
	p_hand_t p_hands[N];
	hand_t hand[N];
	hand_set_t hand_set[N];

	int win = 0;
	int tie = 0;
	int loss = 0;
	int avoid_win = 0;
	int avoid_tie = 0;
	int avoid_loss = 0;
	int fold = 0;

	for (int i = 0; i < 1000; i++) {
		unsigned int seed = time(NULL);
		srand(seed + i * 7);
		poker_deal(N, &table, p_hands);
		for (int n = 0; n < N; n++) {
			poker_best_hand(&table,
					(const p_hand_t*)p_hands + n,
					hand + n,
					hand_set + n);
		}

		bool folding = !chance(ai_weight(hand[0].tag));

		int losing = 0, tying = 0, winning = 0;
		for (int n = 1; n < N; n++) {
			const int res = poker_compare_hand(hand, hand + n);
			if (res == -1)
				losing++;
			if (res == 0)
				tying++;
			if (res == 1)
				winning++;
		}

		if (folding) {
			fold++;
			if (losing > 0)
				avoid_loss++;
			else if (tying > 0)
				avoid_tie++;
			else if (winning > 0)
				avoid_win++;
		} else {
			if (losing > 0)
				loss++;
			else if (tying > 0)
				tie++;
			else if (winning > 0)
				win++;
		}
	}

	printf("win:        %d\ntie:        %d\nloss:       %d\n",
			win, tie, loss);
	printf("avoid_win:  %d\navoid_tie:  %d\navoid_loss: %d\n",
			avoid_win, avoid_tie, avoid_loss);
	printf("fold:       %d\n", fold);

}

int main(int argc, char **argv)
{
	table_t table;
	p_hand_t p_hands[2];
	hand_t hand[2];
	hand_set_t hand_set[2];
		
	unsigned int seed = time(NULL);
	srand(seed * 7);
	poker_deal(2, &table, p_hands);

	poker_best_hand(&table, (const p_hand_t*)p_hands + 0,
			hand + 0, hand_set + 0);

	print_table(&table);
	print_p_hand(p_hands + 0);
	print_hand_tag(hand[0].tag);
	putchar(' ');
	print_hand_set(hand_set + 0);
	puts("");
	return EXIT_SUCCESS;
}
*/
