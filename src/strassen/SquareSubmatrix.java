/*
 * Created on 29/01/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package strassen;

/**
 * @author Glaucio Melo
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */

//	File: SquareSubmatrix.java
import java.io.*;
import java.util.*;
import java.lang.Math;

//	 Determines the largest square submatrices of a matrix of 0s and 1s
//	 such that each submatrix consists entirely either of 0s or of 1s.
//	 Reads matrix from a file listing one row per line.

public class SquareSubmatrix {

	public static void main(String[] arg) {

		if (arg.length < 1)
			System.out.println("Missing argument: Input file name");
		else {
			SquareSubmatrix S = new SquareSubmatrix(arg[0]);
			S.WriteResults();
		}
	}


	public SquareSubmatrix(String file) {
		fileName = file;
		matrix = new ArrayList();
	}

	public void WriteResults() {
		if (ReadInputFile()) {
			FindRank();
			getInfoVector();
			writeInfoVector();
		}
	}

	private boolean OpenInputFile() {
		try {
			buffer = new BufferedReader(new FileReader(new File(fileName)));
			Parser = new StreamTokenizer(buffer);
			Parser.eolIsSignificant(true);
			Parser.slashSlashComments(true);
			return true;
		} catch (IOException e) {
			System.out.println("Error opening file: " + fileName);
			return false;
		}
	}

	private boolean ReadInputFile() {
		if (OpenInputFile()) {
			if (!GetMatrix()) {
				System.out.println(
					"Error reading input matrix from " + fileName);
				return false;
			}
		} else
			return false;
		PrintInputMatrix();
		return true;
	}

	private void PrintInputMatrix() {
		System.out.println("The input matrix is");
		for (int m = 0; m < RowSize; ++m) {
			for (int n = 0; n < ColumnSize; ++n)
				System.out.print(Matrix[m][n] + " ");
			System.out.println();
		}
	}

	private boolean getColumnSize() {
		ColumnSize = 0;
		int k;
		try {
			while ((k = Parser.nextToken()) != StreamTokenizer.TT_EOF) {
				if (k == StreamTokenizer.TT_EOL && ColumnSize > 0)
					break;
				else if (k == StreamTokenizer.TT_NUMBER) {
					++ColumnSize;
					matrix.add(new Integer((short) Parser.nval));
				}
			}
		} catch (Exception e) {
			System.out.println("Error reading a matrix row");
		}
		return ColumnSize > 0;
	}

	private boolean GetMatrix() {
		if (!getColumnSize())
			return false;
		int k;

		try {
			while ((k = Parser.nextToken()) != StreamTokenizer.TT_EOF) {
				if (k == StreamTokenizer.TT_NUMBER)
					matrix.add(new Integer((short) Parser.nval));
			}
			buffer.close();
		} catch (Exception e) {
			System.out.println("Error reading input matrix");
		}

		if (matrix.size() % ColumnSize != 0) {
			System.out.println("Error in input matrix dimensions");
			return false;
		}

		RowSize = matrix.size() / ColumnSize;
		Matrix = new short[RowSize][ColumnSize];

		k = -1;
		for (int m = 0; m < RowSize; ++m)
			for (int n = 0; n < ColumnSize; ++n)
				Matrix[m][n] = ((Integer) matrix.get(++k)).shortValue();
		return true;
	}

	private void FindRank() {
		Rank = new int[RowSize][ColumnSize];
		short value;
		int r, c;
		for (int m = 0; m < RowSize; ++m)
			for (int n = 0; n < ColumnSize; ++n) {
				value = Matrix[m][n];
				r = m + 1;
				while (r < RowSize && Matrix[r][n] == value)
					++r;
				c = n + 1;
				while (c < ColumnSize && Matrix[m][c] == value)
					++c;
				Rank[m][n] = Math.min(r - m, c - n);
			}
	}

	private void getInfoVector() {
		InfoVector = new info[RowSize * ColumnSize];
		int pivot, rank, pivotsRemaining;
		int count = -1;
		short value;
		for (int m = 0; m < RowSize; ++m) {
			for (int n = 0; n < ColumnSize; ++n) {
				pivot = 1;
				rank = Rank[m][n];
				pivotsRemaining = rank;
				value = Matrix[m][n];
				while ((pivotsRemaining =
					Math.min(pivotsRemaining - 1, rank - 1))
					!= 0) {
					if (value != Matrix[m + pivot][n + pivot])
						break;
					rank = Rank[m + pivot][n + pivot];
					++pivot;
				}
				InfoVector[++count] = new info(m, n, pivot, value);
			}
		}
	}

	private void writeInfoVector() {
		Arrays.sort(InfoVector);
		int rank = InfoVector[0].rank;
		int last = 0;
		while (last < InfoVector.length && rank == InfoVector[last].rank)
			++last;

		System.out.println(
			"Largest single-valued square submatrix is "
				+ rank
				+ " by "
				+ rank);
		for (int p = 0; p < last; ++p) {
			System.out.print("Row " + (1 + InfoVector[p].nrow) + "\t");
			System.out.print("Column " + (1 + InfoVector[p].ncol) + "\t");
			System.out.println("Value = " + InfoVector[p].value);
		}
	}

	private int RowSize, ColumnSize;
	private String fileName;
	private BufferedReader buffer;
	private StreamTokenizer Parser;
	private ArrayList matrix;
	private short[][] Matrix;
	private int[][] Rank;
	private info[] InfoVector;
}

class info implements Comparable {
	info(int nrow, int ncol, int rank, short value) {
		this.nrow = nrow;
		this.ncol = ncol;
		this.rank = rank;
		this.value = value;
	}

	public int compareTo(Object x) {
		if (((info) x).rank < rank)
			return -1;
		else if (((info) x).rank > rank)
			return 1;
		else
			return 0;
	}

	public int nrow, ncol, rank;
	public short value;
}
