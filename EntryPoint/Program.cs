using Microsoft.Xna.Framework;
using System;
using System.Collections.Generic;
using System.Linq;

namespace EntryPoint
{
#if WINDOWS || LINUX
  public static class Program
  {
    [STAThread]
    static void Main()
    {
      var fullscreen = false;
      read_input:
      switch (Microsoft.VisualBasic.Interaction.InputBox("Which assignment shall run next? (1, 2, 3, 4, or q for quit)", "Choose assignment", VirtualCity.GetInitialValue()))
      {
        case "1":
          using (var game = VirtualCity.RunAssignment1(SortSpecialBuildingsByDistance, fullscreen))
            game.Run();
          break;
        case "2":
          using (var game = VirtualCity.RunAssignment2(FindSpecialBuildingsWithinDistanceFromHouse, fullscreen))
            game.Run();
          break;
        case "3":
          using (var game = VirtualCity.RunAssignment3(FindRoute, fullscreen))
            game.Run();
          break;
        case "4":
          using (var game = VirtualCity.RunAssignment4(FindRoutesToAll, fullscreen))
            game.Run();
          break;
        case "q":
          return;
      }
      goto read_input;
    }

    private static IEnumerable<Vector2> SortSpecialBuildingsByDistance(Vector2 house, IEnumerable<Vector2> specialBuildings)
    {
            //old code: return specialBuildings.OrderBy(v => Vector2.Distance(v, house));

            //Find the size of the specialBuildings list
            int max = specialBuildings.Count(); 

            //Sort the specialBuildings list
            return SortMerge(specialBuildings.ToArray(), house, 0, max - 1);
        }

    static public void DoMerge(Vector2[] specialBuildings, Vector2 house, int left, int mid, int right)
    {
        Vector2[] temp = new Vector2[specialBuildings.Count()];
        int i, leftEnd, num, pos;

        leftEnd = (mid - 1);
        pos = left;
        num = (right - left + 1);

        while ((left <= leftEnd) && (mid <= right))
        {
            //use the distance method which takes the house and the special building as input and gives back the distance
            if (Vector2.Distance(house, specialBuildings.ElementAt(left)) <= Vector2.Distance(house, specialBuildings.ElementAt(mid)))
                temp[pos++] = specialBuildings[left++];
            else
                temp[pos++] = specialBuildings[mid++];
        }

        while (left <= leftEnd)
            temp[pos++] = specialBuildings[left++];

        while (mid <= right)
            temp[pos++] = specialBuildings[mid++];

        for (i = 0; i < num; i++)
        {
            specialBuildings[right] = temp[right];
            right--;
        }
    }

    static public Vector2[] SortMerge(Vector2[] specialBuildings, Vector2 house, int left, int right)
    {
        int mid;

        if (right > left)
        {
            mid = (right + left) / 2;
            SortMerge(specialBuildings, house, left, mid);
            SortMerge(specialBuildings, house, (mid + 1), right);

            DoMerge(specialBuildings, house, left, (mid + 1), right);
        }
        return specialBuildings;
    }

    private static IEnumerable<IEnumerable<Vector2>> FindSpecialBuildingsWithinDistanceFromHouse(
        IEnumerable<Vector2> specialBuildings,
        IEnumerable<Tuple<Vector2, float>> housesAndDistances)
    {
        //Old function
        //return
        //    from h in housesAndDistances
        //    select
        //      from s in specialBuildings
        //      where Vector2.Distance(h.Item1, s) <= h.Item2
        //      select s;

        var t = new Empty<Vector2>() as KDTree<Vector2>;
        foreach (Vector2 v in specialBuildings)
        {
            t = Insert(t, v, t.isDepthEven);
        }

        List<List<Vector2>> specialBuildingsNearHousesList = new List<List<Vector2>>();
        foreach (Tuple<Vector2, float> h in housesAndDistances)
        {
            List<Vector2> specialBuildingNearSingleHouseList = new List<Vector2>();
            SearchElement(t, h.Item1, h.Item2, specialBuildingNearSingleHouseList);
            specialBuildingsNearHousesList.Add(specialBuildingNearSingleHouseList);
        }

        PrintPreOrder(t, 0);

        return specialBuildingsNearHousesList;
    }

    interface KDTree<T>
    {
        bool IsEmpty { get; }
        bool isDepthEven { get; }
        T vector { get; }
        KDTree<T> Left { get; }
        KDTree<T> Right { get; }
    }

    class Empty<T> : KDTree<T>
    {
        public bool IsEmpty
        {
            get
            {
                return true;
            }
        }

        public bool isDepthEven
        {
            get
            {
                return false;
            }
        }


        public KDTree<T> Left
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        public KDTree<T> Right
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        public T vector
        {
            get
            {
                throw new NotImplementedException();
            }
        }
    }

    class Node<T> : KDTree<T>
    {
        public bool IsEmpty
        {
            get
            {
                return false;
            }
        }

        public bool isDepthEven { get; set; }

        public KDTree<T> Left { get; set; }

        public KDTree<T> Right { get; set; }

        public T vector { get; set; }

        public Node(KDTree<T> l, T v, KDTree<T> r, bool d)
        {
            vector = v;
            Left = l;
            Right = r;
            isDepthEven = d;
        }
    }

    static KDTree<Vector2> Insert(KDTree<Vector2> t, Vector2 v, bool isParentDepthEven)
    {
        if (t.IsEmpty)
        {
            if (isParentDepthEven)
                return new Node<Vector2>(new Empty<Vector2>(), v, new Empty<Vector2>(), false);
            else
                return new Node<Vector2>(new Empty<Vector2>(), v, new Empty<Vector2>(), true);
        }

        if (t.vector == v)
            return t;

        if (t.isDepthEven)
        {
            if (v.X < t.vector.X)
                return new Node<Vector2>(Insert(t.Left, v, t.isDepthEven), t.vector, t.Right, true);
            else
                return new Node<Vector2>(t.Left, t.vector, Insert(t.Right, v, t.isDepthEven), true);
        }
        else
        {
            if (v.Y < t.vector.Y)
                return new Node<Vector2>(Insert(t.Left, v, t.isDepthEven), t.vector, t.Right, false);
            else
                return new Node<Vector2>(t.Left, t.vector, Insert(t.Right, v, t.isDepthEven), false);
        }
    }

    static void SearchElement(KDTree<Vector2> t, Vector2 v, float radius, List<Vector2> searchResultList)
    {
        if (!t.IsEmpty)
        {
            if (t.isDepthEven)
            {
                if (Math.Abs(t.vector.X - v.X) <= radius)
                {
                    if (Vector2.Distance(t.vector, v) <= radius)
                        searchResultList.Add(t.vector);
                    SearchElement(t.Left, v, radius, searchResultList);
                    SearchElement(t.Right, v, radius, searchResultList);
                }

                else if (t.vector.X > (v.X + radius))
                    SearchElement(t.Left, v, radius, searchResultList);
                else if (t.vector.X < (v.X - radius))
                    SearchElement(t.Right, v, radius, searchResultList);
            }
            else
            {
                if (Math.Abs(t.vector.Y - v.Y) <= radius)
                {
                    if (Vector2.Distance(t.vector, v) <= radius)
                        searchResultList.Add(t.vector);
                    SearchElement(t.Left, v, radius, searchResultList);
                    SearchElement(t.Right, v, radius, searchResultList);
                }
                else if (t.vector.Y > (v.Y + radius))
                    SearchElement(t.Left, v, radius, searchResultList);
                else if (t.vector.Y < (v.Y - radius))
                    SearchElement(t.Right, v, radius, searchResultList);
            }
        }
    }

    static void PrintPreOrder(KDTree<Vector2> t, int depth)
    {
        if (t.IsEmpty) return;
        Console.WriteLine("depth: " + depth + ", vector: " + t.vector);
        PrintPreOrder(t.Left, depth + 1);
        PrintPreOrder(t.Right, depth + 1);
    }

    static void PrintInOrder(KDTree<Vector2> t, int depth)
    {
        if (t.IsEmpty) return;
        PrintInOrder(t.Left, depth + 1);
        Console.WriteLine("depth: " + depth + ", vector: " + t.vector);
        PrintInOrder(t.Right, depth + 1);
    }

    private static IEnumerable<Tuple<Vector2, Vector2>> FindRoute(Vector2 startingBuilding, 
    Vector2 destinationBuilding, IEnumerable<Tuple<Vector2, Vector2>> roads)
        {
      var startingRoad = roads.Where(x => x.Item1.Equals(startingBuilding)).First();
      List<Tuple<Vector2, Vector2>> fakeBestPath = new List<Tuple<Vector2, Vector2>>() { startingRoad };
      var prevRoad = startingRoad;
      for (int i = 0; i < 30; i++)
      {
        prevRoad = (roads.Where(x => x.Item1.Equals(prevRoad.Item2)).OrderBy(x => Vector2.Distance(x.Item2, destinationBuilding)).First());
        fakeBestPath.Add(prevRoad);
      }
      return fakeBestPath;
    }

    private static IEnumerable<IEnumerable<Tuple<Vector2, Vector2>>> FindRoutesToAll(Vector2 startingBuilding, 
      IEnumerable<Vector2> destinationBuildings, IEnumerable<Tuple<Vector2, Vector2>> roads)
    {
      List<List<Tuple<Vector2, Vector2>>> result = new List<List<Tuple<Vector2, Vector2>>>();
      foreach (var d in destinationBuildings)
      {
        var startingRoad = roads.Where(x => x.Item1.Equals(startingBuilding)).First();
        List<Tuple<Vector2, Vector2>> fakeBestPath = new List<Tuple<Vector2, Vector2>>() { startingRoad };
        var prevRoad = startingRoad;
        for (int i = 0; i < 30; i++)
        {
          prevRoad = (roads.Where(x => x.Item1.Equals(prevRoad.Item2)).OrderBy(x => Vector2.Distance(x.Item2, d)).First());
          fakeBestPath.Add(prevRoad);
        }
        result.Add(fakeBestPath);
      }
      return result;
    }
  }
#endif
}
