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
      return
          from h in housesAndDistances
          select
            from s in specialBuildings
            where Vector2.Distance(h.Item1, s) <= h.Item2
            select s;
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
