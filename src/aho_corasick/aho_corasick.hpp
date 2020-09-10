/*
* Copyright (C) 2018 Christopher Gilbert.
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
*/

#ifndef AHO_CORASICK_HPP
#define AHO_CORASICK_HPP

#include <algorithm>
#include <cctype>
#include <map>
#include <memory>
#include <set>
#include <list>
#include <string>
#include <queue>
#include <utility>
#include <vector>
#include <unordered_map>

namespace aho_corasick {
	// class state
	template<typename CharType>
	class state {
	public:
		typedef state<CharType>*                 ptr;
		typedef std::unique_ptr<state<CharType>> unique_ptr;
		typedef std::basic_string<CharType>      string_type;
		typedef std::basic_string<CharType>&     string_ref_type;
		typedef std::pair<unsigned, unsigned> 	 key_index;
		typedef std::list<key_index>             index_collection;
		typedef std::vector<ptr>                 state_collection;
		typedef std::vector<CharType>            transition_collection;

	private:
		size_t                         d_depth;
		ptr                            d_root;
		std::map<CharType, unique_ptr> d_success;
		ptr                            d_failure;
		bool						   d_has_keyword;
		index_collection               d_emits;

	public:
		state(): state(0) {}

		explicit state(size_t depth)
			: d_depth(depth)
			, d_root(depth == 0 ? this : nullptr)
			, d_success()
			, d_failure(nullptr)
			, d_has_keyword(false)
			, d_emits() {}

		ptr next_state(CharType character) const {
			return next_state(character, false);
		}

		ptr next_state_ignore_root_state(CharType character) const {
			return next_state(character, true);
		}

		ptr add_state(CharType character) {
			auto next = next_state_ignore_root_state(character);
			if (next == nullptr) {
				next = new state<CharType>(d_depth + 1);
				d_success[character].reset(next);
			}
			return next;
		}

		void set_has_keyword() { d_has_keyword = true; }

		bool has_keyword() { return d_has_keyword; }

		size_t get_depth() const { return d_depth; }

		void add_emit(unsigned keyword_index, unsigned position) {
			d_emits.push_back(std::make_pair(keyword_index, position));
		}

		void add_emit(const index_collection& emits) {
			for (const auto& e : emits) {
				add_emit(e.first, e.second);
			}
		}

		index_collection get_emits() const { return d_emits; }

		ptr failure() const { return d_failure; }

		void set_failure(ptr fail_state) { d_failure = fail_state; }

		state_collection get_states() const {
			state_collection result;
			for (auto it = d_success.cbegin(); it != d_success.cend(); ++it) {
				result.push_back(it->second.get());
			}
			return state_collection(result);
		}

		transition_collection get_transitions() const {
			transition_collection result;
			for (auto it = d_success.cbegin(); it != d_success.cend(); ++it) {
				result.push_back(it->first);
			}
			return transition_collection(result);
		}

	private:
		ptr next_state(CharType character, bool ignore_root_state) const {
			ptr result = nullptr;
			auto found = d_success.find(character);
			if (found != d_success.end()) {
				result = found->second.get();
			} else if (!ignore_root_state && d_root != nullptr) {
				result = d_root;
			}
			return result;
		}
	};

	template<typename CharType>
	class basic_trie {
	public:
		using string_type = std::basic_string < CharType > ;
		using string_ref_type = std::basic_string<CharType>&;
		typedef enum match_type{ FULL, PREFIX, SUFFIX } match_type;

		typedef state<CharType>         											   state_type;
		typedef state<CharType>*        											   state_ptr_type;
		typedef std::unordered_map<unsigned, std::unordered_map<match_type, unsigned>> emit_collection;

		class config {
			bool d_case_insensitive;

		public:
			config()
				: d_case_insensitive(false) {}

			bool is_case_insensitive() const { return d_case_insensitive; }
			void set_case_insensitive(bool val) { d_case_insensitive = val; }
		};

	private:
		std::unique_ptr<state_type> d_root;
		config                      d_config;
		bool                        d_constructed_failure_states;
		unsigned                    d_num_keywords = 0;

	public:
		basic_trie(): basic_trie(config()) {}

		basic_trie(const config& c)
			: d_root(new state_type())
			, d_config(c)
			, d_constructed_failure_states(false) {}

		basic_trie& case_insensitive() {
			d_config.set_case_insensitive(true);
			return (*this);
		}

		void insert(string_type keyword, unsigned keyword_index, bool reverse) {
			if (keyword.empty())
				return;
			state_ptr_type cur_state = d_root.get();
			unsigned length = 0;
			for (const auto& ch : keyword) {
				length++;
				cur_state = cur_state->add_state(ch);
				cur_state->add_emit(keyword_index, length);
			}
			if (!reverse)
			    cur_state->set_has_keyword();
			d_constructed_failure_states = false;
		}

		emit_collection parse_text(string_type text, bool stop_at_full, emit_collection& collected_emits) {
			check_construct_failure_states();
			size_t pos = 0;
			state_ptr_type cur_state = d_root.get();
			for (auto c : text) {
				if (d_config.is_case_insensitive()) {
					c = std::tolower(c);
				}
				cur_state = get_state(cur_state, c);
				pos++;
				if (cur_state->has_keyword()) {
				    store_emits(pos, cur_state, collected_emits, stop_at_full, FULL);
				}
				else if (pos == text.size()) {
				    store_emits(pos, cur_state, collected_emits, stop_at_full, PREFIX);
				}
			}
			return emit_collection(collected_emits);
		}

		emit_collection parse_reverse_text(string_type text, bool stop_at_full, emit_collection& collected_emits) {
			check_construct_failure_states();
			size_t pos = text.size()-1;
			state_ptr_type cur_state = d_root.get();
			for (auto rit=text.rbegin(); rit!=text.rend(); ++rit) {
				if (d_config.is_case_insensitive()) {
					*rit = std::tolower(*rit);
				}
				cur_state = get_state(cur_state, *rit);
				pos--;
				if (pos == -1) {
				    store_emits(pos, cur_state, collected_emits, stop_at_full, SUFFIX);
				}
			}
			return emit_collection(collected_emits);
		}

	private:
		state_ptr_type get_state(state_ptr_type cur_state, CharType c) const {
			state_ptr_type result = cur_state->next_state(c);
			while (result == nullptr) {
				cur_state = cur_state->failure();
				result = cur_state->next_state(c);
			}
			return result;
		}

		void check_construct_failure_states() {
			if (!d_constructed_failure_states) {
				construct_failure_states();
			}
		}

		void construct_failure_states() {
			std::queue<state_ptr_type> q;
			for (auto& depth_one_state : d_root->get_states()) {
				depth_one_state->set_failure(d_root.get());
				q.push(depth_one_state);
			}
			d_constructed_failure_states = true;

			while (!q.empty()) {
				auto cur_state = q.front();
				for (const auto& transition : cur_state->get_transitions()) {
					state_ptr_type target_state = cur_state->next_state(transition);
					q.push(target_state);

					state_ptr_type trace_failure_state = cur_state->failure();
					while (trace_failure_state->next_state(transition) == nullptr) {
						trace_failure_state = trace_failure_state->failure();
					}
					state_ptr_type new_failure_state = trace_failure_state->next_state(transition);
					target_state->set_failure(new_failure_state);
					target_state->add_emit(new_failure_state->get_emits());
				}
				q.pop();
			}
		}

		void store_emits(size_t pos, state_ptr_type cur_state, emit_collection& collected_emits, bool stop_at_full, match_type mtype) const {
			auto emits = cur_state->get_emits();
			if (!emits.empty()) {
				for (const auto& emit : emits) {
					if (collected_emits[emit.first].find(FULL) == collected_emits[emit.first].end() || (mtype != FULL && !stop_at_full)) {
				        collected_emits[emit.first].insert(std::make_pair(mtype, emit.second));
					}
				}
			}
		}
	};

	typedef basic_trie<char>     trie;
	typedef basic_trie<wchar_t>  wtrie;


} // namespace aho_corasick

#endif // AHO_CORASICK_HPP


