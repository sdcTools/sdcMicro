#include <Rcpp.h>
using namespace Rcpp;

// Faithful port of the R reference loop in ls_greedy2() (R/ls_greedy2.R):
// identical repairs, identical deterministic tie-breaks, identical
// S1-deployed counting including the contributor-liveness clause. The R
// engine stays in the package as the reference; test-ls_greedy2.R pins
// strict equality of the two engines.

static bool credits(const IntegerMatrix& cur, int p, int b,
                    const std::vector<int>& rel, bool rel_incomp) {
  for (int j = 0; j < p; ++j) {
    int rv = rel[j], ov = cur(b, j);
    if (rv != NA_INTEGER && ov != NA_INTEGER && rv != ov) return false;
  }
  bool allna = true;
  for (int j = 0; j < p; ++j) {
    if (cur(b, j) != NA_INTEGER) { allna = false; break; }
  }
  if (allna && !rel_incomp) return false;   // liveness clause
  return true;
}

static double count_with(int a, int mk, double mass_mk,
                         const IntegerMatrix& vm, const IntegerMatrix& cur,
                         const std::vector<int>& keys0, int nV, int p) {
  std::vector<int> rel(p);
  for (int j = 0; j < p; ++j) rel[j] = vm(a, j);
  for (size_t b = 0; b < keys0.size(); ++b) {
    if (mk & (1 << b)) rel[keys0[b]] = NA_INTEGER;
  }
  bool rel_incomp = false;
  for (int j = 0; j < p; ++j) {
    if (rel[j] == NA_INTEGER) { rel_incomp = true; break; }
  }
  double cnt = 1.0 + mass_mk;
  for (int b = 0; b < nV; ++b) {
    if (b == a) continue;
    if (credits(cur, p, b, rel, rel_incomp)) cnt += 1.0;
  }
  return cnt;
}

// [[Rcpp::export]]
List cpp_greedy2_core(IntegerMatrix vm, List patKeys, List patMasks,
                      List patCost, List patMass, NumericVector k) {
  const int nV = vm.nrow(), p = vm.ncol();
  IntegerMatrix cur = clone(vm);
  std::vector<int> supp(nV, 0);
  std::vector<std::vector<int> > keys0(nV), masks(nV);
  std::vector<std::vector<double> > cost(nV), mass(nV);
  for (int a = 0; a < nV; ++a) {
    IntegerVector kk = patKeys[a];
    for (int i = 0; i < kk.size(); ++i) keys0[a].push_back(kk[i] - 1);
    IntegerVector mm = patMasks[a];
    masks[a].assign(mm.begin(), mm.end());
    NumericVector cc = patCost[a];
    cost[a].assign(cc.begin(), cc.end());
    NumericVector ms = patMass[a];
    mass[a].assign(ms.begin(), ms.end());
  }
  const double eps = 1e-9;
  int steps = 0;

  // index of mask value within a violator's (cost-ordered) pattern arrays
  auto idx_of = [&](int a, int mk) {
    for (size_t q = 0; q < masks[a].size(); ++q) {
      if (masks[a][q] == mk) return static_cast<int>(q);
    }
    return -1;
  };

  for (;;) {
    std::vector<int> unresolved;
    for (int a = 0; a < nV; ++a) {
      int q = idx_of(a, supp[a]);
      if (count_with(a, supp[a], mass[a][q], vm, cur, keys0[a], nV, p) < k[a]) {
        unresolved.push_back(a);
      }
    }
    if (unresolved.empty()) break;

    int best_a = -1, best_mk = 0, best_size = 0;
    double best_extra = 0.0;
    for (size_t u = 0; u < unresolved.size(); ++u) {
      int a = unresolved[u];
      int found_mk = -1, found_size = 0;
      double found_cost = 0.0;
      for (size_t q = 0; q < masks[a].size(); ++q) {
        int mk = masks[a][q] | supp[a];
        if (mk != masks[a][q]) continue;          // supersets only
        if (count_with(a, mk, mass[a][q], vm, cur, keys0[a], nV, p) >= k[a]) {
          found_mk = mk;
          found_cost = cost[a][q];
          for (size_t b = 0; b < keys0[a].size(); ++b) {
            if (mk & (1 << b)) ++found_size;
          }
          break;                                   // cheapest first
        }
      }
      if (found_mk < 0) continue;
      double extra = found_cost - cost[a][idx_of(a, supp[a])];
      bool better = best_a < 0 || extra < best_extra - eps ||
        (std::abs(extra - best_extra) < eps && found_size < best_size) ||
        (std::abs(extra - best_extra) < eps && found_size == best_size &&
         a < best_a);
      if (better) {
        best_a = a; best_mk = found_mk;
        best_extra = extra; best_size = found_size;
      }
    }
    if (best_a < 0) stop("greedy2: no repair found for the remaining violators.");
    supp[best_a] = best_mk;
    for (size_t b = 0; b < keys0[best_a].size(); ++b) {
      if (best_mk & (1 << b)) cur(best_a, keys0[best_a][b]) = NA_INTEGER;
    }
    ++steps;
  }

  return List::create(_["supp_mask"] = wrap(supp), _["steps"] = steps);
}
