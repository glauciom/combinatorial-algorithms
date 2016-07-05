package br.com.gm2.routines.java.random;

import java.util.Arrays;

import org.junit.Assert;
import org.junit.Test;

public class RandomPermutationTest {

	@Test
	public void checkRandomPermutation() {
		int k = 100;
		for (int y = 0; y < k; y++) {
			int n = ((int) (Math.random() * k)) + 2;
			int[] identity = new int[n];
			for (int i = 0; i < n; i++) {
				identity[i] = i + 1;
			}
			RandomPermutation test = new RandomPermutation(n);
			test.randomPermutationAlgorithm();
			Arrays.sort(test.getRandomPermutation());
			Assert.assertArrayEquals(identity, test.getRandomPermutation());
		}
	}

}
