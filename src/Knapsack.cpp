#include <cstdio>
#include <cstring>
#include <iostream>

// currentVolume: Das restliche (freie) Volumen im Rucksack
// i: Aktueller Gegenstand
// Return value: Maximaler Wert mit bzw. ohne den aktuellen Gegenstand
int knapsack( const int currentVolume, const int i );

// Maximale Anazahl an Gegenständen
const int maxN = 10;
// Maximales Volumen des Rucksacks
const int maxV = 30;
// Anzahl an Byte für unseren Dynamisierungs-Array
const int resultLength = ( maxV + 1 ) * sizeof( int );

// Volumen des Rucksacks
int V;
// Anzahl an Gegenständen
int n;
// Volumen der einzelnen Gegenstände
int v[maxN];
// Werte der einzelnen Gegenstände
int w[maxN];
// Gespeicherte Ergebnisse von bisherigen Berechnungen
int results[maxN][maxV + 1];

int main()
{
  // Eingabe der Daten
  fscanf( stdin, "%d %d", &V, &n );
  for( int i = 0; i < n; i++ )
    fscanf( stdin, "%d %d", &( v[i] ), &( w[i] ) );
  // Initialisierung der Dynamisierungs-Matrix mit -1
  for( int i = 0; i < n; i++ )
    memset( results[i], -1, resultLength );
  // Berechnung des optimalen Wertes
  int r = knapsack( V, 0 );
  // Belegtes Volumen beim optimalen Ergebnis suchen.
  // Das optimale Ergebnis befindet sich immer in der ersten Spalte!
  int currentVolume = -1;
  for( int i = 0; i <= maxV && currentVolume == -1; i++ )
  {
    if( results[0][i] == r )
      currentVolume = i;
  }
  // Über alle Gegenstände iterieren (-1, weil mit i+1 gerechnet wird)
  for( int i = 0; i < maxN - 1; i++ )
  {
    // Wenn der Wert beim nächten Gegenstands-Index und
    // bei gleichem Gewicht ein anderer ist,
    // gehoert der aktuelle Gegenstand in den Rucksack.
    if( results[i][currentVolume] != results[i + 1][currentVolume] )
    {
      // Gegenstand war im Rucksack
      fprintf( stdout, "selecting %d (%d / %d)\n", i + 1, v[i], w[i] );
      currentVolume -= v[i];
    }
    else
    {
      // Gegenstand war nicht im Rucksack
      fprintf( stdout, "NOT selecting %d (%d / %d)\n", i + 1, v[i], w[i] );
    }
  }
  return 0;
}


int knapsack( const int currentVolume, const int i )
{
  if( i < n )
  {
    if( results[i][currentVolume] != -1 )
      return results[i][currentVolume];
    return results[i][currentVolume] = std::max( knapsack( currentVolume, i + 1 ),
                                                 ( currentVolume - v[i] >= 0 )
                                                 ? ( w[i] + knapsack( currentVolume - v[i], i + 1 ) )
                                                 : 0 );
  }
  return 0;
}