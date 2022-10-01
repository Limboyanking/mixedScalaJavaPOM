package leetCode;


import java.util.*;

class ListNode {
    int val;
    ListNode next;

    ListNode(int x) {
        val = x;
    }
}

class TreeNode {
    int val;
    TreeNode left;
    TreeNode right;

    TreeNode(int x) {
        val = x;
    }
}

public class JavaSolution {
    //18
    int len = 0;

    public List<List<Integer>> fourSum(int[] nums, int target) {
        len = nums.length;
        Arrays.sort(nums);
        return kSum(nums, target, 4, 0);
    }

    private ArrayList<List<Integer>> kSum(int[] nums, int target, int k, int index) {
        ArrayList<List<Integer>> res = new ArrayList<List<Integer>>();
        if (index >= len) {
            return res;
        }
        if (k == 2) {
            int i = index, j = len - 1;
            while (i < j) {
                //find a pair
                if (target - nums[i] == nums[j]) {
                    List<Integer> temp = new ArrayList<Integer>();
                    temp.add(nums[i]);
                    temp.add(target - nums[i]);
                    res.add(temp);
                    //skip duplication
                    while (i < j && nums[i] == nums[i + 1]) i++;
                    while (i < j && nums[j - 1] == nums[j]) j--;
                    i++;
                    j--;
                    //move left bound
                } else if (target - nums[i] > nums[j]) {
                    i++;
                    //move right bound
                } else {
                    j--;
                }
            }
        } else {
            System.out.println("\n" + "index(start): " + index + " end: " + (len - k));
            for (int i = index; i < len - k + 1; i++) {
                //use current number to reduce ksum into k-1sum
                System.out.println("\n" + "curIndex i + 1: " + (i + 1) + " target: " + target + " curTarget: " + (target - nums[i]) + " cur K: " + (k - 1));
                ArrayList<List<Integer>> temp = kSum(nums, target - nums[i], k - 1, i + 1);

                if (temp != null) {
                    //add previous results
                    for (List<Integer> t : temp) {
                        System.out.println("pre t: " + t + " i: " + i);
                        t.add(0, nums[i]);
                        System.out.println("t: " + t);
                    }
                    res.addAll(temp);
                }
                while (i < len - 1 && nums[i] == nums[i + 1]) {
                    //skip duplicated numbers
                    i++;
                }
            }
        }
        return res;
    }

    //22 给定N对 ( ) , 找出所有有效的组合
    public List<String> generateParenthesis(int n) {
        List<List<String>> lists = new ArrayList<List<String>>();
        lists.add(Collections.singletonList(""));

        for (int i = 1; i <= n; ++i) {
            final List<String> list = new ArrayList<String>();

            for (int j = 0; j < i; ++j) {
                for (final String first : lists.get(j)) {
                    for (final String second : lists.get(i - 1 - j)) {
                        list.add("(" + first + ")" + second);
                    }
                }
            }

            lists.add(list);
        }

        return lists.get(lists.size() - 1);
    }

    //23 将k条有序的ListNode链表，整合成一条
    private static ListNode partion(ListNode[] lists, int s, int e) {
        if (s == e) return lists[s];
        if (s < e) {
            int q = (s + e) / 2;
            ListNode l1 = partion(lists, s, q);
            ListNode l2 = partion(lists, q + 1, e);
//            return merge(l1,l2);
            return new ListNode(0);
        } else
            return null;
    }

    //24 将链表中每两个相邻的节点交换位置
    public ListNode swapPairs(ListNode head) {
        if (head == null || head.next == null) return head;
        ListNode second = head.next;
        ListNode third = second.next;

        second.next = head;
        head.next = swapPairs(third);

        return second;
    }

    public ListNode swapPair(ListNode head) {
        if (head == null || head.next == null) return head;
        ListNode newHead = head.next, a = head, b = a.next, pre = null;
        while (a != null && b != null) {
            a.next = b.next;
            b.next = a;
            if (pre != null) pre.next = b;
            if (a.next == null) break;
            b = a.next.next;
            pre = a;
            a = a.next;
        }
        return newHead;
    }

    //239. Sliding Window Maximum
    public int[] maxSlidingWindow(int[] nums, int k) {
        ArrayList<Integer> res = new ArrayList<Integer>();
        ArrayDeque<Integer> arrDeque = new ArrayDeque();

        for (int i = 0, j = 0; i < nums.length; i++) {
            while (!arrDeque.isEmpty() && nums[i] > nums[arrDeque.peekFirst()])
                arrDeque.removeFirst();
            arrDeque.addFirst(i);

            if (i - k + 1 > arrDeque.peekLast()) {
                arrDeque.removeLast();
                j++;
            }
            if (i - j + 1 == k)
                res.add(nums[arrDeque.peekLast()]);
        }

        int[] r = new int[res.size()];
        int ri = 0;
        for (Integer e : res) {
            r[ri] = e;
            ri++;
        }

        return r;
    }

    //95. Unique Binary Search Trees ||
    public List<TreeNode> generateTrees(int n) {
        if (n == 0)
            return new ArrayList<TreeNode>();

        ArrayList<TreeNode> ans = getBST(1, n);

        return ans;
    }

    ArrayList<TreeNode> getBST(int l, int r) {
        ArrayList<TreeNode> res = new ArrayList<TreeNode>();

        //down to bottom
        //null should be recorded then go up
        if (l > r) {
            res.add(null);
            return res;
        } else {
            //recursively down to get subRanges's BSTs
            for (int i = l; i <= r; i++) {
                ArrayList<TreeNode> leftBSTs = getBST(l, i - 1);
                ArrayList<TreeNode> rightBSTs = getBST(i + 1, r);
                //get current digit's BSTs
                for (TreeNode lt : leftBSTs) {
                    for (TreeNode rt : rightBSTs) {
                        //form a new tree that belongs to current digit
                        TreeNode node = new TreeNode(i);
                        node.left = lt;
                        node.right = rt;
                        //add the new tree to current range
                        res.add(node);
                    }
                }
            }

            return res;
        }
    }


    //394. Decode String
    /*
      considering the pattern (no endless recurse)

      [0-9]+
      \[
        [A-z]+  exists or not
        [0-9]+  must exist
        \[
        [A-z]+
        \]
      \]

      traversing String s

      when [0-9]+\[ occurs ,
      push current repeatTimes and current ans into stack,
      i.e a range which
      starts from current "[" to the symmetrical or corresponding "]"
      flush current ans

      when \] occurs,
      pop out previous ans and current repeatTimes
      then updating previous ans by appending current ans repeatedly
      ex. 2[a3[bc]] => 2[abcbcbc]
          previous ans = "a"
          current repeatTimes = 3
          current ans = "bc"
          updated ans = “abcbcbc”

      when letter occurs,
      appending to current ans

    */
    public String decodeString(String s) {
        StringBuilder ans = new StringBuilder();
        StringBuilder sb = new StringBuilder();
        int i = 0, j = 0;
        while (i < s.length() && j < s.length()) {
            //deal with letters in front of [0-9]+\[
            if (s.charAt(i) >= 'A' && s.charAt(i) <= 'z') {
                ans.append(s.charAt(i));
                i++;
            } else {
                //deal with digits
                int k = 0;
                while (Character.isDigit(s.charAt(i))) {
                    k = k * 10 + s.charAt(i) - '0';
                    i++;
                }

                //deal with '[' then go for symmetrical ']'
                j = i + 1;
                int sum = 1;
                while (sum > 0) {
                    if (s.charAt(j) == '[')
                        sum++;
                    if (s.charAt(j) == ']')
                        sum--;
                    j++;
                }

                //deal with ']'
                //dfs tree go down for inner result of current range
                // i i + 1            j
                // [                ]
                String curRes = decodeString(s.substring(i + 1, j - 1));
                while (k > 0) {
                    ans.append(curRes);
                    k--;
                }
                i = j;
            }
        }

        return ans.toString();
    }

    public boolean pyramidTransition(String bottom, List<String> allowed) {
        ArrayList[][] allowRela = new ArrayList[7][7];
        for (int i = 0; i < 7; i++)
            for (int j = 0; j < 7; j++)
                allowRela[i][j] = new ArrayList<Character>();

        for (String a : allowed) {
            int x, y = 0;
            char z = ' ';
            x = a.charAt(0) - 'A';
            y = a.charAt(1) - 'A';
            z = a.charAt(2);
            allowRela[x][y].add(z);
        }

        ArrayDeque<String []> queue = new ArrayDeque<>();
        queue.push(new String[]{bottom,""});

        while(!queue.isEmpty()){
            String [] head = queue.pop();
            String pre = head[0];
            String cur = head[1];
            if(pre.length() == 1)
                return true;

            //searching for possible path
            // cur  0 1 ?
            // pre 0 1 2 3
            int x,y = 0;
            x = pre.charAt(cur.length()) - 'A';
            y = pre.charAt(cur.length() + 1) - 'A';
            for(Object c : allowRela[x][y]){
                char cc = (Character) c;
                String s = cur + cc;
                //reaching top
                if(s.length() == 1 && pre.length() == 2)
                    return true;
                if(s.length() + 1 == pre.length())
                    queue.add(new String[]{s,""});
                else
                    queue.add(new String[]{pre,s});
            }
        }

        return false;
    }


}
