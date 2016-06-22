/*
 * Created on 16/05/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package partition;

/**
 * @author Glaucio Melo
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class NextPartitionNSet {

	private int n;
	private int[] p, q;
	private int nc, m, l;

	public NextPartitionNSet(int n) {
		this.n = n;
		this.p = new int[n];
		this.q = new int[n];
		p[0] = n;
	}

	public void algorithm() {
		int temp;
		if (nc == n - 1)
			return;
		for (m = n-1, l = q[m]; p[l] == 1; q[m] = 0, m--, l = q[m]);	
		
		nc += m - (n - 1);
		p[0] += (n - 1) - m;
		temp = l == nc ? p[++nc] = 0 : l; 
		q[m] = l + 1;
		p[l]--;
		p[l + 1]++;
	}

	public boolean isLastPartition() {
		return nc == n - 1;
	}

	public String getOutput() {
		String s = "";
		for (int i = 0; i < q.length; i++)
			s += (q[i]) + " ";
		return s;
	}
	
	public int[] getIndexes() {
		return q;
	}

	public String toString() {
		String s[] = new String[n];
		String result = "";
		for (int i = 0; i < s.length; i++) {
			s[i] = "";
		}
		for (int i = 0; i < q.length; i++) {
			s[q[i]] += (i + 1) + ",";
		}

		for (int i = 0; i < s.length; i++) {
			String k = "";
			if (s[i].length() > 1)
				k = s[i].substring(0, s[i].length() - 1);
			s[i] = "(" + k + ")";
			if (!s[i].equals("()"))
				result += s[i];
		}
		return result;
	}

	public static void main(String[] args) {
		NextPartitionNSet test = new NextPartitionNSet(5);
		int count = 1;
		System.out.println(count++ + "-> "+test.getOutput());
		while (!test.isLastPartition()) {
			test.algorithm();
			System.out.println(count + "-> "+test.getOutput());
			count++;
		}

	}
}