#pragma once

#include "../rsx_cache.h"
#include "../rsx_utils.h"
#include "texture_cache_utils.h"
#include "TextureUtils.h"

#include <atomic>

extern u64 get_system_time();

namespace rsx
{
	template <typename commandbuffer_type, typename section_storage_type, typename image_resource_type, typename image_view_type, typename image_storage_type, typename texture_format>
	class texture_cache : public rsx::texture_cache_base<section_storage_type>
	{
		static_assert(std::is_base_of<rsx::cached_texture_section<section_storage_type>, section_storage_type>::value, "section_storage_type must derive from rsx::cached_texture_section");

	public:
		using superclass           = typename rsx::texture_cache_base<section_storage_type>;
		using ranged_storage       = typename rsx::ranged_storage<section_storage_type>;
		using ranged_storage_block = typename ranged_storage::block_type;

	private:
		/**
		 * Helper structs/enums
		 */

		// Keep track of cache misses to pre-emptively flush some addresses
		struct framebuffer_memory_characteristics
		{
			u32 misses;
			u32 block_size;
			texture_format format;
		};

	public:
		//Struct to hold data on sections to be paged back onto cpu memory
		struct thrashed_set
		{
			bool violation_handled = false;
			std::vector<section_storage_type*> sections_to_flush; // Sections to be flushed
			std::vector<section_storage_type*> sections_to_unprotect; // These sections are to be unpotected and discarded by caller
			std::vector<section_storage_type*> sections_to_exclude; // These sections are do be excluded from protection manipulation (subtracted from other sections)
			int num_flushable = 0;
			u64 cache_tag = 0;
			address_range fault_range;
		};

		struct intersecting_set
		{
			std::vector<section_storage_type*> sections;
			address_range invalidate_range;
		};

		struct copy_region_descriptor
		{
			image_resource_type src;
			u16 src_x;
			u16 src_y;
			u16 dst_x;
			u16 dst_y;
			u16 dst_z;
			u16 w;
			u16 h;
		};

		enum deferred_request_command : u32
		{
			copy_image_static,
			copy_image_dynamic,
			cubemap_gather,
			cubemap_unwrap,
			atlas_gather,
			_3d_gather,
			_3d_unwrap
		};

		using texture_channel_remap_t = std::pair<std::array<u8, 4>, std::array<u8, 4>>;
		struct deferred_subresource
		{
			image_resource_type external_handle = 0;
			std::vector<copy_region_descriptor> sections_to_copy;
			texture_channel_remap_t remap;
			deferred_request_command op;
			u32 base_address = 0;
			u32 gcm_format = 0;
			u16 x = 0;
			u16 y = 0;
			u16 width = 0;
			u16 height = 0;
			u16 depth = 1;

			deferred_subresource()
			{}

			deferred_subresource(image_resource_type _res, deferred_request_command _op, u32 _addr, u32 _fmt, u16 _x, u16 _y, u16 _w, u16 _h, u16 _d, const texture_channel_remap_t& _remap) :
				external_handle(_res), op(_op), base_address(_addr), gcm_format(_fmt), x(_x), y(_y), width(_w), height(_h), depth(_d), remap(_remap)
			{}
		};

		struct blit_op_result
		{
			bool succeeded = false;
			bool is_depth = false;
			u32 real_dst_address = 0;
			u32 real_dst_size = 0;

			blit_op_result(bool success) : succeeded(success)
			{}

			inline address_range to_address_range() const
			{
				return address_range::create_start_length(real_dst_address, real_dst_size);
			}
		};

		struct sampled_image_descriptor : public sampled_image_descriptor_base
		{
			image_view_type image_handle = 0;
			deferred_subresource external_subresource_desc = {};
			bool flag = false;

			sampled_image_descriptor()
			{}

			sampled_image_descriptor(image_view_type handle, texture_upload_context ctx, bool is_depth, f32 x_scale, f32 y_scale, rsx::texture_dimension_extended type)
			{
				image_handle = handle;
				upload_context = ctx;
				is_depth_texture = is_depth;
				scale_x = x_scale;
				scale_y = y_scale;
				image_type = type;
			}

			sampled_image_descriptor(image_resource_type external_handle, deferred_request_command reason, u32 base_address, u32 gcm_format,
				u16 x_offset, u16 y_offset, u16 width, u16 height, u16 depth, texture_upload_context ctx, bool is_depth, f32 x_scale, f32 y_scale,
				rsx::texture_dimension_extended type, const texture_channel_remap_t& remap)
			{
				external_subresource_desc = { external_handle, reason, base_address, gcm_format, x_offset, y_offset, width, height, depth, remap };

				image_handle = 0;
				upload_context = ctx;
				is_depth_texture = is_depth;
				scale_x = x_scale;
				scale_y = y_scale;
				image_type = type;
			}

			u32 encoded_component_map() const override
			{
				if (image_handle)
				{
					return image_handle->encoded_component_map();
				}

				return 0;
			}
		};


	protected:

		/**
		 * Variable declarations
		 */

		shared_mutex m_cache_mutex;
		ranged_storage m_storage;
		std::unordered_multimap<u32, std::pair<deferred_subresource, image_view_type>> m_temporary_subresource_cache;

		std::atomic<u64> m_cache_update_tag = {0};

		address_range read_only_range;
		address_range no_access_range;

		std::unordered_map<u32, framebuffer_memory_characteristics> m_cache_miss_statistics_table;

		//Map of messages to only emit once
		std::unordered_set<std::string> m_once_only_messages_set;

		//Set when a shader read-only texture data suddenly becomes contested, usually by fbo memory
		bool read_only_tex_invalidate = false;

		//Store of all objects in a flush_always state. A lazy readback is attempted every draw call
		std::unordered_map<u32, u32> m_flush_always_cache;
		u64 m_flush_always_update_timestamp = 0;

		//Memory usage
		const s32 m_max_zombie_objects = 64; //Limit on how many texture objects to keep around for reuse after they are invalidated
		std::atomic<s32> m_unreleased_texture_objects = { 0 }; //Number of invalidated objects not yet freed from memory
		std::atomic<u32> m_texture_memory_in_use = { 0 };

		//Other statistics
		const u32 m_cache_miss_threshold = 8; // How many times an address can miss speculative writing before it is considered high priority
		std::atomic<u32> m_num_flush_requests = { 0 };
		std::atomic<u32> m_num_cache_misses = { 0 };
		std::atomic<u32> m_num_cache_speculative_writes = { 0 };
		std::atomic<u32> m_num_cache_mispredictions = { 0 };

		// Invalidation
		static const invalidation_chain_policy invalidation_policy = invalidation_chain_policy::invalidation_chain_none;
		static const u32 invalidation_chain_nearby_pages = 0;
		static const u32 invalidation_force_nearby_pages = 0;
		static const invalidation_chain_direction invalidation_direction = invalidation_chain_direction::chain_direction_forward;
		static const bool invalidation_ignore_unsynchronized = true; // If true, unsynchronized sections don't get forcefully flushed unless they overlap the fault range


		/**
		 * Virtual Methods
		 */
		virtual void free_texture_section(section_storage_type&) = 0;
		virtual image_view_type create_temporary_subresource_view(commandbuffer_type&, image_resource_type* src, u32 gcm_format, u16 x, u16 y, u16 w, u16 h, const texture_channel_remap_t& remap_vector) = 0;
		virtual image_view_type create_temporary_subresource_view(commandbuffer_type&, image_storage_type* src, u32 gcm_format, u16 x, u16 y, u16 w, u16 h, const texture_channel_remap_t& remap_vector) = 0;
		virtual section_storage_type* create_new_texture(commandbuffer_type&, const address_range &rsx_range, u16 width, u16 height, u16 depth, u16 mipmaps, u32 gcm_format,
				rsx::texture_upload_context context, rsx::texture_dimension_extended type, texture_create_flags flags) = 0;
		virtual section_storage_type* upload_image_from_cpu(commandbuffer_type&, u32 rsx_address, u16 width, u16 height, u16 depth, u16 mipmaps, u16 pitch, u32 gcm_format, texture_upload_context context,
				const std::vector<rsx_subresource_layout>& subresource_layout, rsx::texture_dimension_extended type, bool swizzled) = 0;
		virtual void enforce_surface_creation_type(section_storage_type& section, u32 gcm_format, texture_create_flags expected) = 0;
		virtual void insert_texture_barrier(commandbuffer_type&, image_storage_type* tex) = 0;
		virtual image_view_type generate_cubemap_from_images(commandbuffer_type&, u32 gcm_format, u16 size, const std::vector<copy_region_descriptor>& sources, const texture_channel_remap_t& remap_vector) = 0;
		virtual image_view_type generate_3d_from_2d_images(commandbuffer_type&, u32 gcm_format, u16 width, u16 height, u16 depth, const std::vector<copy_region_descriptor>& sources, const texture_channel_remap_t& remap_vector) = 0;
		virtual image_view_type generate_atlas_from_images(commandbuffer_type&, u32 gcm_format, u16 width, u16 height, const std::vector<copy_region_descriptor>& sections_to_copy, const texture_channel_remap_t& remap_vector) = 0;
		virtual void update_image_contents(commandbuffer_type&, image_view_type dst, image_resource_type src, u16 width, u16 height) = 0;
		virtual bool render_target_format_is_compatible(image_storage_type* tex, u32 gcm_format) = 0;


		/**
		 * Helpers
		 */
		inline void update_cache_tag()
		{
			m_cache_update_tag++;
		}

		template <typename... Args>
		void emit_once(bool error, const char* fmt, const Args&... params)
		{
			const auto result = m_once_only_messages_set.emplace(fmt::format(fmt, params...));
			if (!result.second)
				return;

			if (error)
				LOG_ERROR(RSX, "%s", *result.first);
			else
				LOG_WARNING(RSX, "%s", *result.first);
		}

		template <typename... Args>
		void err_once(const char* fmt, const Args&... params)
		{
			logs::RSX.error(fmt, params...);
		}

		template <typename... Args>
		void warn_once(const char* fmt, const Args&... params)
		{
			logs::RSX.warning(fmt, params...);
		}

		/**
		 * Internal implementation methods and helpers
		 */

		inline bool region_intersects_cache(const address_range &test_range, bool is_writing) const
		{
			AUDIT( test_range.valid() );

			if (!is_writing)
			{
				if (!no_access_range.valid() || !test_range.overlaps(no_access_range))
					return false;
			}
			else
			{
				if (!read_only_range.valid() || !test_range.overlaps(read_only_range))
				{
					//Doesnt fall in the read_only textures range; check render targets
					if (!no_access_range.valid() || !test_range.overlaps(no_access_range))
						return false;
				}
			}

			return true;
		}

		// Merges the protected ranges of the sections in "sections" into "result"
		void merge_protected_ranges(address_range_vector &result, const std::vector<section_storage_type*> &sections)
		{
			result.reserve(result.size() + sections.size());

			// Copy ranges to result, merging them if possible
			for (const auto &section : sections)
			{
				const auto &new_range = section->get_locked_range();
				AUDIT( new_range.is_page_range() );

				result.merge(new_range);
			}
		}

		// NOTE: It is *very* important that data contains exclusions for *all* sections that overlap sections_to_unprotect/flush
		//       Otherwise the page protections will end up incorrect and things will break!
		void unprotect_set(thrashed_set& data)
		{
			auto protect_ranges = [this](address_range_vector& _set, utils::protection _prot)
			{
				u32 count = 0;
				for (auto &range : _set)
				{
					if (range.valid())
					{
						range.protect(_prot);
						count++;
					}
				}
				//LOG_ERROR(RSX, "Set protection of %d blocks to 0x%x", count, static_cast<u32>(prot));
			};

			auto discard_set = [this](std::vector<section_storage_type*>& _set)
			{
				for (auto* section : _set)
				{
					verify(HERE), section->is_flushed() || section->is_dirty();

					const bool dirty = section->is_dirty();
					section->discard();
					section->set_dirty(dirty);
				}
			};


			// Merge ranges to unprotect
			address_range_vector ranges_to_unprotect;
			address_range_vector ranges_to_reprotect_ro;
			ranges_to_unprotect.reserve(data.sections_to_unprotect.size() + data.sections_to_flush.size() + data.sections_to_exclude.size());

			merge_protected_ranges(ranges_to_unprotect, data.sections_to_unprotect);
			merge_protected_ranges(ranges_to_unprotect, data.sections_to_flush);
			AUDIT(!ranges_to_unprotect.empty());

			// Apply exclusions and collect ranges of excluded pages that need to be reprotected RO (i.e. only overlap RO regions)
			if (!data.sections_to_exclude.empty())
			{
				ranges_to_reprotect_ro.reserve(data.sections_to_exclude.size());

				u32 no_access_count = 0;
				for (const auto &excluded : data.sections_to_exclude)
				{
					const auto &exclusion_range = excluded->get_locked_range();
					AUDIT( exclusion_range.is_page_range() );

					// Apply exclusion
					ranges_to_unprotect.exclude(exclusion_range);

					// Keep track of RO exclusions
					utils::protection prot = excluded->get_protection();
					if (prot == utils::protection::ro)
						ranges_to_reprotect_ro.merge(exclusion_range);
					else if (prot == utils::protection::no)
						no_access_count++;
					else
						fmt::throw_exception("Unreachable" HERE);
				}

				// Exclude NA ranges from ranges_to_reprotect_ro
				if (no_access_count > 0 && !ranges_to_reprotect_ro.empty())
				{
					for (auto &exclusion : data.sections_to_exclude)
					{
						if(exclusion->get_protection() != utils::protection::ro)
							ranges_to_reprotect_ro.exclude(exclusion->get_locked_range());
					}
				}
			}
			AUDIT( !ranges_to_unprotect.empty() );

			// Unprotect and discard
			discard_set(data.sections_to_unprotect);
			discard_set(data.sections_to_flush);
			protect_ranges(ranges_to_unprotect, utils::protection::rw);
			protect_ranges(ranges_to_reprotect_ro, utils::protection::ro);

#ifdef TEXTURE_CACHE_DEBUG
			// Check cache sanity
			m_storage.verify_protection();

			// Check that the fault_range is RW (otherwise we could have deadlocks)
			data.fault_range.verify_protection(utils::protection::rw);
#endif // TEXTURE_CACHE_DEBUG
		}

		// Return a set containing all sections that should be flushed/unprotected/reprotected
		intersecting_set get_intersecting_set(const address_range &fault_range)
		{
			AUDIT( fault_range.is_page_range() );

			const u64 cache_tag = get_system_time();

			intersecting_set result = {};
			address_range &invalidate_range = result.invalidate_range;
			invalidate_range = fault_range; // Sections fully inside this range will be invalidated, others will be deemed false positives

			// Add nearby pages to invalidation range
			if (invalidation_force_nearby_pages > 0)
			{
				if (invalidation_direction != invalidation_chain_direction::chain_direction_backward)
				{
					// Increase the invalidation range tail
					invalidate_range.end += invalidation_force_nearby_pages * 4096u;
				}
				if (invalidation_direction != invalidation_chain_direction::chain_direction_forward)
				{
					// Increase the invalidation range head
					invalidate_range.start -= invalidation_force_nearby_pages * 4096u;
				}
			}

			// Loop through cache and find pages that overlap the invalidate_range
			u32 last_dirty_block = UINT32_MAX;
			bool repeat_loop = false;

			auto It = m_storage.range_begin(invalidate_range, full_range);
			while (It != m_storage.range_end())
			{
				const u32 base = It.get_block().get_start();

				// On the last loop, we stop once we're done with the last dirty block
				if (!repeat_loop && base > last_dirty_block) // note: blocks are iterated in order from lowest to highest base address
					break;

				auto &tex = *It;

				AUDIT(tex.overlaps(invalidate_range, full_range));
				AUDIT(tex.cache_tag != cache_tag || last_dirty_block != UINT32_MAX); // cache tag should not match during the first loop

				if (tex.cache_tag != cache_tag && //not yet processed
					tex.is_locked()) //flushable sections can be 'clean' but unlocked. TODO: Handle this better
				{
					const rsx::section_bounds bounds = tex.get_overlap_test_bounds();

					if (bounds == It.get_bounds() || tex.overlaps(invalidate_range, bounds))
					{
						const auto new_range = tex.get_min_max(invalidate_range, bounds).to_page_range();
						AUDIT(new_range.is_page_range() && invalidate_range.inside(new_range));

						const s32 signed_distance = tex.signed_distance(fault_range, section_bounds::locked_range);
						const s32 distance = signed_distance < 0 ? -signed_distance : signed_distance;
						const bool is_after_fault = (signed_distance >= 0);

						// The various chaining policies behave differently
						bool extend_invalidate_range = tex.overlaps(fault_range, bounds);

						if (invalidation_policy == invalidation_chain_policy::invalidation_chain_none)
						{
							// No chaining
						}
						else if (invalidation_ignore_unsynchronized && tex.is_flushable() && !tex.is_synchronized())
						{
							// Unsynchronized and does not overlap the fault range
						}
						else if ((!is_after_fault && invalidation_direction == invalidation_chain_direction::chain_direction_forward) ||
							(is_after_fault && invalidation_direction == invalidation_chain_direction::chain_direction_backward))
						{
							// invalidation_direction mismatch
						}
						else if (invalidation_policy == invalidation_chain_policy::invalidation_chain_full)
						{
							// Chaining always enabled
							extend_invalidate_range = true;
						}
						else if (invalidation_policy == invalidation_chain_policy::invalidation_chain_nearby)
						{
							// We invalidate if this section is near to the fault range
							extend_invalidate_range |= (distance <= (4096u * invalidation_chain_nearby_pages));
						}
						else
						{
							fmt::throw_exception("Unreachable" HERE);
						}

						// Extend the various ranges
						if (extend_invalidate_range && new_range != invalidate_range)
						{
							if (new_range.end > invalidate_range.end)
								It.set_end(new_range.end);

							invalidate_range = new_range;
							repeat_loop = true; // we will need to repeat the loop again
							last_dirty_block = base; // stop the repeat loop once we finish this block
						}

						// Add texture to result, and update its cache tag
						tex.cache_tag = cache_tag;
						result.sections.push_back(&tex);
					}
				}

				// Iterate
				It++;

				// repeat_loop==true means some blocks are still dirty and we need to repeat the loop again
				if (repeat_loop && It == m_storage.range_end())
				{
					It = m_storage.range_begin(invalidate_range, full_range);
					repeat_loop = false;
				}
			}

			AUDIT( result.invalidate_range.is_page_range() );

#ifdef TEXTURE_CACHE_DEBUG
			// naive check that sections are not duplicated
			for (auto &section1 : result.sections)
			{
				size_t count = 0;
				for(auto &section2 : result.sections)
				{
					if (section1 == section2) count++;
				}
				verify(HERE), count == 1;
			}
#endif //TEXTURE_CACHE_DEBUG

			return result;
		}

		//Invalidate range base implementation
		template <typename ...Args>
		thrashed_set invalidate_range_impl_base(const address_range &fault_range_in, bool is_writing, bool discard_only, bool allow_flush, Args&&... extras)
		{
			AUDIT( fault_range_in.valid() );
			const address_range fault_range = fault_range_in.to_page_range();

			if (!region_intersects_cache(fault_range, is_writing))
				return {};

			auto trampled_set = get_intersecting_set(fault_range);

			if (trampled_set.sections.size() > 0)
			{
				update_cache_tag();

				bool deferred_flush = !discard_only && !allow_flush;

				thrashed_set result = {};
				result.violation_handled = true;

				for (auto &obj : trampled_set.sections)
				{
					auto &tex = *obj;

					if (!discard_only)
					{
						const rsx::section_bounds bounds = tex.get_overlap_test_bounds();

						// Sections that are not fully contained in invalidate_range can be ignored
						// Unsynchronized sections that do not overlap the fault range directly can also be ignored
						if (!tex.inside(trampled_set.invalidate_range, bounds) ||
							(invalidation_ignore_unsynchronized && tex.is_flushable() && !tex.is_synchronized() && !tex.overlaps(fault_range, bounds)))
						{
							// False positive
							result.sections_to_exclude.push_back(&tex);
							continue;
						}

						if (tex.is_flushable())
						{
							//Write if and only if no one else has trashed section memory already
							//TODO: Proper section management should prevent this from happening
							//TODO: Blit engine section merge support and/or partial texture memory buffering
							if (!tex.test_memory_head() || !tex.test_memory_tail())
							{
								//Contents clobbered, destroy this
								if (!tex.is_dirty())
								{
									tex.set_dirty(true);
									m_unreleased_texture_objects++;
								}

								result.sections_to_unprotect.push_back(&tex);
							}
							else if (!allow_flush)
							{
								result.sections_to_flush.push_back(&tex);
							}
							else
							{
								if (tex.get_memory_read_flags() == rsx::memory_read_flags::flush_always)
								{
									// This region is set to always read from itself (unavoidable hard sync)
									const auto ROP_timestamp = rsx::get_current_renderer()->ROP_sync_timestamp;
									if (tex.is_synchronized() && ROP_timestamp > tex.get_sync_timestamp())
									{
										m_num_cache_mispredictions++;
										m_num_cache_misses++;
										tex.copy_texture(true, std::forward<Args>(extras)...);
									}
								}

								if (!tex.flush(std::forward<Args>(extras)...))
								{
									//Missed address, note this
									//TODO: Lower severity when successful to keep the cache from overworking
									record_cache_miss(tex);
								}

								m_num_flush_requests++;
								result.sections_to_unprotect.push_back(&tex);
							}

							continue;
						}
						else
						{
							//allow_flush = false and not synchronized
							if (!tex.is_dirty())
							{
								AUDIT( tex.get_memory_read_flags() != memory_read_flags::flush_always );
								tex.set_dirty(true);
								m_unreleased_texture_objects++;
							}

							result.sections_to_unprotect.push_back(&tex);
							continue;
						}
					}

					verify(HERE), discard_only;

					m_unreleased_texture_objects++;

					tex.discard();
				}

				result.fault_range = fault_range;

				if (discard_only)
				{
					AUDIT( !deferred_flush && result.sections_to_flush.empty() && result.sections_to_unprotect.empty() && result.sections_to_exclude.empty() );
				}
				else if (deferred_flush && result.sections_to_flush.size())
				{
					result.num_flushable = static_cast<int>(result.sections_to_flush.size());
					result.cache_tag = m_cache_update_tag.load(std::memory_order_consume);
					return result;
				}
				else
				{
					AUDIT( !result.sections_to_flush.empty() || !result.sections_to_unprotect.empty() );
					unprotect_set(result);
				}

				//Everything has been handled
				result = {};
				result.violation_handled = true;
				return result;
			}

			return {};
		}


		inline bool is_hw_blit_engine_compatible(u32 format) const
		{
			switch (format)
			{
			case CELL_GCM_TEXTURE_A8R8G8B8:
			case CELL_GCM_TEXTURE_R5G6B5:
			case CELL_GCM_TEXTURE_DEPTH16:
			case CELL_GCM_TEXTURE_DEPTH24_D8:
				return true;
			default:
				return false;
			}
		}

		/**
		 * Scaling helpers
		 * - get_native_dimensions() returns w and h for the native texture given rsx dimensions
		 *   on rsx a 512x512 texture with 4x AA is treated as a 1024x1024 texture for example
		 * - get_rsx_dimensions() inverse, return rsx w and h given a real texture w and h
		 * - get_internal_scaling_x/y() returns a scaling factor to be multiplied by 1/size
		 *   when sampling with unnormalized coordinates. tcoords passed to rsx will be in rsx dimensions
		 */
		template <typename T, typename U>
		inline void get_native_dimensions(T &width, T &height, U surface)
		{
			switch (surface->read_aa_mode)
			{
			case rsx::surface_antialiasing::center_1_sample:
				return;
			case rsx::surface_antialiasing::diagonal_centered_2_samples:
				width /= 2;
				return;
			case rsx::surface_antialiasing::square_centered_4_samples:
			case rsx::surface_antialiasing::square_rotated_4_samples:
				width /= 2;
				height /= 2;
				return;
			}
		}

		template <typename T, typename U>
		inline void get_rsx_dimensions(T &width, T &height, U surface)
		{
			switch (surface->read_aa_mode)
			{
			case rsx::surface_antialiasing::center_1_sample:
				return;
			case rsx::surface_antialiasing::diagonal_centered_2_samples:
				width *= 2;
				return;
			case rsx::surface_antialiasing::square_centered_4_samples:
			case rsx::surface_antialiasing::square_rotated_4_samples:
				width *= 2;
				height *= 2;
				return;
			}
		}

		template <typename T>
		inline f32 get_internal_scaling_x(T surface)
		{
			switch (surface->read_aa_mode)
			{
			default:
			case rsx::surface_antialiasing::center_1_sample:
				return 1.f;
			case rsx::surface_antialiasing::diagonal_centered_2_samples:
			case rsx::surface_antialiasing::square_centered_4_samples:
			case rsx::surface_antialiasing::square_rotated_4_samples:
				return 0.5f;
			}
		}

		template <typename T>
		inline f32 get_internal_scaling_y(T surface)
		{
			switch (surface->read_aa_mode)
			{
			default:
			case rsx::surface_antialiasing::center_1_sample:
			case rsx::surface_antialiasing::diagonal_centered_2_samples:
				return 1.f;
			case rsx::surface_antialiasing::square_centered_4_samples:
			case rsx::surface_antialiasing::square_rotated_4_samples:
				return 0.5f;
			}
		}

	public:

		texture_cache() : m_storage(this) {}
		~texture_cache() {}

		virtual void destroy() = 0;
		virtual bool is_depth_texture(u32, u32) = 0;
		virtual void on_frame_end() = 0;

		std::vector<section_storage_type*> find_texture_from_range(const address_range &test_range)
		{
			std::vector<section_storage_type*> results;

			for (auto It = m_storage.range_begin(test_range, full_range); It != m_storage.range_end(); It++)
			{
				auto &tex = *It;

				// TODO ruipin: Removed as a workaround for a bug, will need to be fixed by kd-11
				//if (tex.get_section_base() > test_range.start)
				//	continue;

				if (!tex.is_dirty())
					results.push_back(&tex);
			}

			return results;
		}

		section_storage_type *find_texture_from_dimensions(u32 rsx_address, u16 width = 0, u16 height = 0, u16 depth = 0, u16 mipmaps = 0)
		{
			auto &block = m_storage.block_for(rsx_address);
			for (auto &tex : block)
			{
				if (tex.matches(rsx_address, width, height, depth, mipmaps) && !tex.is_dirty())
				{
					return &tex;
				}
			}

			return nullptr;
		}

		section_storage_type& find_cached_texture(const address_range &range, bool confirm_dimensions = false, u16 width = 0, u16 height = 0, u16 depth = 0, u16 mipmaps = 0)
		{
			auto &block = m_storage.block_for(range);

			section_storage_type *best_fit = nullptr;
			section_storage_type *first_dirty = nullptr;

			// Try to find match in block
			for (auto &tex : block)
			{
				if (tex.matches(range))
				{
					if (!tex.is_dirty())
					{
						if (!confirm_dimensions || tex.matches_dimensions(width, height, depth, mipmaps))
						{
							return tex;
						}
						else
						{
							LOG_ERROR(RSX, "Cached object for address 0x%X was found, but it does not match stored parameters. %d x %d vs %d x %d", range.start, width, height, tex.get_width(), tex.get_height());
						}
					}
					else if (best_fit == nullptr)
					{
						//By grabbing a ref to a matching entry, duplicates are avoided
						best_fit = &tex;
					}
				}
				else if(first_dirty == nullptr && tex.is_dirty())
				{
					AUDIT(!tex.is_locked());
					first_dirty = &tex;
				}
			}

			// If found, use the best fitting section
			if (best_fit)
			{
				AUDIT( !best_fit->is_locked() );

				if (best_fit->exists())
				{
					if (best_fit->get_context() != rsx::texture_upload_context::framebuffer_storage)
						m_texture_memory_in_use -= best_fit->get_section_size();

					m_unreleased_texture_objects--;
					free_texture_section(*best_fit);
				}

				return *best_fit;
			}

			// Return the first dirty section found, if any
			if (first_dirty != nullptr)
			{
				if (first_dirty->exists())
				{
					if (first_dirty->get_context() != rsx::texture_upload_context::framebuffer_storage)
						m_texture_memory_in_use -= first_dirty->get_section_size();

					m_unreleased_texture_objects--;
					free_texture_section(*first_dirty);
				}

				return *first_dirty;
			}

			// Create and return a new section
			update_cache_tag();
			auto &tex = block.create_section();
			return tex;
		}

		section_storage_type* find_flushable_section(const address_range &memory_range)
		{
			auto &block = m_storage.block_for(memory_range);
			for (auto &tex : block)
			{
				if (tex.is_dirty()) continue;
				if (!tex.is_flushable() && !tex.is_flushed()) continue;

				if (tex.matches(memory_range))
					return &tex;
			}

			return nullptr;
		}

		template <typename ...Args>
		void lock_memory_region(image_storage_type* image, const address_range &rsx_range, u32 width, u32 height, u32 pitch, Args&&... extras)
		{
			AUDIT( g_cfg.video.write_color_buffers ); // this method is only called when WCB is enabled

			std::lock_guard lock(m_cache_mutex);

			section_storage_type& region = find_cached_texture(rsx_range, false);

			if (region.get_context() != texture_upload_context::framebuffer_storage &&
				region.exists())
			{
				//This space was being used for other purposes other than framebuffer storage
				//Delete used resources before attaching it to framebuffer memory
				read_only_tex_invalidate = true;
				free_texture_section(region);
				m_texture_memory_in_use -= region.get_section_size();
			}

			region.discard();
			region.reset(rsx_range);
			region.set_dirty(false);
			no_access_range = region.get_min_max(no_access_range, rsx::section_bounds::locked_range);

			region.create(width, height, 1, 1, image, pitch, false, std::forward<Args>(extras)...);
			region.set_context(texture_upload_context::framebuffer_storage);
			region.set_image_type(rsx::texture_dimension_extended::texture_dimension_2d);
			region.set_memory_read_flags(memory_read_flags::flush_always);
			region.touch();

			// Test for invalidated sections from surface cache occupying same address range
			const auto mem_range = rsx_range.to_page_range();
			const auto &overlapped = find_texture_from_range(mem_range);
			AUDIT( !overlapped.empty() );

			bool found_region = false;
			for (auto* surface : overlapped)
			{
				if (surface == &region)
				{
					found_region = true;
					continue;
				}

				if (surface->get_context() != rsx::texture_upload_context::framebuffer_storage)
				{
					m_unreleased_texture_objects++;
				}
				else
				{
					if (surface->get_section_base() != rsx_range.start)
						// HACK: preserve other overlapped sections despite overlap unless new section is superseding
						// TODO: write memory to cell or redesign sections to preserve the data
						// TODO ruipin: can this be done now?
						continue;
				}

				// Memory is shared with another surface
				// Discard it - the backend should ensure memory contents are preserved if needed
				surface->set_dirty(true);

				if (surface->is_locked())
				{
					AUDIT(surface->get_memory_read_flags() != memory_read_flags::flush_always);
					if (surface->is_flushable() && surface->test_memory_head() && surface->test_memory_tail())
					{
						if (!surface->overlaps(mem_range, rsx::section_bounds::full_range))
						{
							// TODO: This section contains data that should be flushed
							LOG_TODO(RSX, "Flushable section data may have been lost (0x%x)", surface->get_section_base());
						}
					}

					// TODO ruipin: is this safe? What about NA/RO regions overlapping this one?
					//              What about the small time period between this unprotect and the reprotect below?
					surface->unprotect();
				}
			}
			verify(HERE), found_region;

			// Delay protection until here in case the invalidation block above has unprotected pages in this range
			region.reprotect(utils::protection::no, { 0, rsx_range.length() });
			update_cache_tag();
		}

		void set_memory_read_flags(const address_range &memory_range, memory_read_flags flags)
		{
			std::lock_guard lock(m_cache_mutex);

			section_storage_type& region = find_cached_texture(memory_range, false);

			if (!region.exists() || region.get_context() != texture_upload_context::framebuffer_storage)
			{
#ifdef TEXTURE_CACHE_DEBUG
				if (flags == memory_read_flags::flush_once)
					verify(HERE), m_flush_always_cache.find(memory_range.start) == m_flush_always_cache.end();
				else
					verify(HERE), m_flush_always_cache[memory_range.start] == memory_range.length();
#endif
				return;
			}

			update_flush_always_cache(region, flags == memory_read_flags::flush_always);
			region.set_memory_read_flags(flags, false);
		}

		virtual void on_memory_read_flags_changed(const section_storage_type &section, rsx::memory_read_flags flags)
		{
#ifdef TEXTURE_CACHE_DEBUG
			const auto &memory_range = section.get_section_range();
			if (flags == memory_read_flags::flush_once)
				verify(HERE), m_flush_always_cache[memory_range.start] == memory_range.length();
			else
				verify(HERE), m_flush_always_cache.find(memory_range.start) == m_flush_always_cache.end();
#endif
			update_flush_always_cache(section, flags == memory_read_flags::flush_always);
		}

	private:
		inline void update_flush_always_cache(const section_storage_type &section, bool add)
		{
			const address_range& range = section.get_section_range();
			if (add)
			{
				// Add to m_flush_always_cache
				AUDIT(m_flush_always_cache.find(range.start) == m_flush_always_cache.end());
				m_flush_always_cache[range.start] = range.length();
			}
			else
			{
				// Remove from m_flush_always_cache
				AUDIT(m_flush_always_cache[range.start] == range.length());
				m_flush_always_cache.erase(range.start);
			}
		}

	public:
		template <typename ...Args>
		bool flush_memory_to_cache(const address_range &memory_range, bool skip_synchronized, u32 allowed_types_mask, Args&&... extra)
		{
			std::lock_guard lock(m_cache_mutex);
			section_storage_type* region = find_flushable_section(memory_range);

			//Check if section was released, usually if cell overwrites a currently bound render target
			if (region == nullptr)
				return true;

			if (skip_synchronized && region->is_synchronized())
				return false;

			if ((allowed_types_mask & region->get_context()) == 0)
				return true;

			if (!region->writes_likely_completed())
				return true;

			region->copy_texture(false, std::forward<Args>(extra)...);
			m_num_cache_speculative_writes++;
			return true;
		}

		template <typename ...Args>
		bool load_memory_from_cache(const address_range &memory_range, Args&&... extras)
		{
			reader_lock lock(m_cache_mutex);
			section_storage_type *region = find_flushable_section(memory_range);

			if (region && !region->is_dirty())
			{
				region->fill_texture(std::forward<Args>(extras)...);
				return true;
			}

			//No valid object found in cache
			return false;
		}

		template <typename ...Args>
		thrashed_set invalidate_address(u32 address, bool is_writing, bool allow_flush, Args&&... extras)
		{
			//Test before trying to acquire the lock
			const auto range = page_for(address);
			if (!region_intersects_cache(range, is_writing))
				return{};

			std::lock_guard lock(m_cache_mutex);
			return invalidate_range_impl_base(range, is_writing, false, allow_flush, std::forward<Args>(extras)...);
		}

		template <typename ...Args>
		thrashed_set invalidate_range(const address_range &range, bool is_writing, bool discard, bool allow_flush, Args&&... extras)
		{
			//Test before trying to acquire the lock
			if (!region_intersects_cache(range, is_writing))
				return {};

			std::lock_guard lock(m_cache_mutex);
			return invalidate_range_impl_base(range, is_writing, discard, allow_flush, std::forward<Args>(extras)...);
		}

		template <typename ...Args>
		bool flush_all(thrashed_set& data, Args&&... extras)
		{
			std::lock_guard lock(m_cache_mutex);

			if (m_cache_update_tag.load(std::memory_order_consume) == data.cache_tag)
			{
				//1. Write memory to cpu side
				for (auto &tex : data.sections_to_flush)
				{
					if (tex->is_locked())
					{
						if (tex->get_memory_read_flags() == rsx::memory_read_flags::flush_always)
						{
							// This region is set to always read from itself (unavoidable hard sync)
							const auto ROP_timestamp = rsx::get_current_renderer()->ROP_sync_timestamp;
							if (tex->is_synchronized() && ROP_timestamp > tex->get_sync_timestamp())
							{
								m_num_cache_mispredictions++;
								m_num_cache_misses++;
								tex->copy_texture(true, std::forward<Args>(extras)...);
							}
						}

						if (!tex->flush(std::forward<Args>(extras)...))
						{
							record_cache_miss(*tex);
						}

						m_num_flush_requests++;
					}
				}

				//2. Release all obsolete sections
				unprotect_set(data);
			}
			else
			{
				// The cache contents have changed between the two readings. This means the data held is useless
				invalidate_range_impl_base(data.fault_range, true, false, true, std::forward<Args>(extras)...);
			}

			return true;
		}

		void record_cache_miss(section_storage_type &tex)
		{
			m_num_cache_misses++;

			const u32 memory_address = tex.get_section_base();
			const u32 memory_size = tex.get_section_size();
			const auto fmt = tex.get_format();

			auto It = m_cache_miss_statistics_table.find(memory_address);
			if (It == m_cache_miss_statistics_table.end())
			{
				m_cache_miss_statistics_table[memory_address] = { 1, memory_size, fmt };
				return;
			}

			auto &value = It->second;
			if (value.format != fmt || value.block_size != memory_size)
			{
				m_cache_miss_statistics_table[memory_address] = { 1, memory_size, fmt };
				return;
			}

			value.misses += 2;
		}

		template <typename ...Args>
		bool flush_if_cache_miss_likely(texture_format fmt, const address_range &memory_range, Args&&... extras)
		{
			auto It = m_cache_miss_statistics_table.find(memory_range.start);
			if (It == m_cache_miss_statistics_table.end())
			{
				m_cache_miss_statistics_table[memory_range.start] = { 0, memory_range.length(), fmt };
				return false;
			}

			auto &value = It->second;

			if (value.format != fmt || value.block_size < memory_range.length())
			{
				//Reset since the data has changed
				//TODO: Keep track of all this information together
				m_cache_miss_statistics_table[memory_range.start] = { 0, memory_range.length(), fmt };
			}

			// By default, blit targets are always to be tested for readback
			u32 flush_mask = rsx::texture_upload_context::blit_engine_dst;

			// Auto flush if this address keeps missing (not properly synchronized)
			if (value.misses >= m_cache_miss_threshold)
			{
				// Disable prediction if memory is flagged as flush_always
				if (m_flush_always_cache.find(memory_range.start) == m_flush_always_cache.end())
				{
					// TODO: Determine better way of setting threshold
					// Allow all types
					flush_mask = 0xFF;
				}
			}

			if (!flush_memory_to_cache(memory_range, true, flush_mask, std::forward<Args>(extras)...) &&
				value.misses > 0)
			{
				value.misses--;
			}

			return true;
		}

		void purge_dirty()
		{
			std::lock_guard lock(m_cache_mutex);

			//Reclaims all graphics memory consumed by dirty textures
			size_t count = 0;
			for (auto &block : m_storage)
			{
				for (auto &tex : block)
				{
					if (!tex.is_dirty())
						continue;

					if (tex.exists() &&
						tex.get_context() != rsx::texture_upload_context::framebuffer_storage)
					{
						free_texture_section(tex);
						m_texture_memory_in_use -= tex.get_section_size();
						count++;
					}
				}
			}

			// TODO ruipin: This fails, most likely a bug
			//AUDIT(count == m_unreleased_texture_objects);
			m_unreleased_texture_objects = 0;
		}

		image_view_type create_temporary_subresource(commandbuffer_type &cmd, deferred_subresource& desc)
		{
			const auto found = m_temporary_subresource_cache.equal_range(desc.base_address);
			for (auto It = found.first; It != found.second; ++It)
			{
				const auto& found_desc = It->second.first;
				if (found_desc.external_handle != desc.external_handle ||
					found_desc.op != desc.op ||
					found_desc.x != desc.x || found_desc.y != desc.y ||
					found_desc.width != desc.width || found_desc.height != desc.height)
					continue;

				if (desc.op == deferred_request_command::copy_image_dynamic)
					update_image_contents(cmd, It->second.second, desc.external_handle, desc.width, desc.height);

				return It->second.second;
			}

			image_view_type result = 0;
			switch (desc.op)
			{
			case deferred_request_command::cubemap_gather:
			{
				result = generate_cubemap_from_images(cmd, desc.gcm_format, desc.width, desc.sections_to_copy, desc.remap);
				break;
			}
			case deferred_request_command::cubemap_unwrap:
			{
				std::vector<copy_region_descriptor> sections(6);
				for (u16 n = 0; n < 6; ++n)
				{
					sections[n] = { desc.external_handle, 0, (u16)(desc.height * n), 0, 0, n, desc.width, desc.height };
				}

				result = generate_cubemap_from_images(cmd, desc.gcm_format, desc.width, sections, desc.remap);
				break;
			}
			case deferred_request_command::_3d_gather:
			{
				result = generate_3d_from_2d_images(cmd, desc.gcm_format, desc.width, desc.height, desc.depth, desc.sections_to_copy, desc.remap);
				break;
			}
			case deferred_request_command::_3d_unwrap:
			{
				std::vector<copy_region_descriptor> sections;
				sections.resize(desc.depth);
				for (u16 n = 0; n < desc.depth; ++n)
				{
					sections[n] = { desc.external_handle, 0, (u16)(desc.height * n), 0, 0, n, desc.width, desc.height };
				}

				result = generate_3d_from_2d_images(cmd, desc.gcm_format, desc.width, desc.height, desc.depth, sections, desc.remap);
				break;
			}
			case deferred_request_command::atlas_gather:
			{
				result = generate_atlas_from_images(cmd, desc.gcm_format, desc.width, desc.height, desc.sections_to_copy, desc.remap);
				break;
			}
			case deferred_request_command::copy_image_static:
			case deferred_request_command::copy_image_dynamic:
			{
				result = create_temporary_subresource_view(cmd, &desc.external_handle, desc.gcm_format, desc.x, desc.y, desc.width, desc.height, desc.remap);
				break;
			}
			default:
			{
				//Throw
				fmt::throw_exception("Invalid deferred command op 0x%X" HERE, (u32)desc.op);
			}
			}

			if (result)
			{
				m_temporary_subresource_cache.insert({ desc.base_address,{ desc, result } });
			}

			return result;
		}

		void notify_surface_changed(u32 base_address)
		{
			m_temporary_subresource_cache.erase(base_address);
		}

		template<typename surface_store_type>
		std::vector<copy_region_descriptor> gather_texture_slices_from_framebuffers(u32 texaddr, u16 slice_w, u16 slice_h, u16 pitch, u16 count, u8 bpp, surface_store_type& m_rtts)
		{
			std::vector<copy_region_descriptor> surfaces;
			u32 current_address = texaddr;
			u32 slice_size = (pitch * slice_h);
			bool unsafe = false;

			for (u16 slice = 0; slice < count; ++slice)
			{
				auto overlapping = m_rtts.get_merged_texture_memory_region(current_address, slice_w, slice_h, pitch, bpp);
				current_address += (pitch * slice_h);

				if (overlapping.empty())
				{
					unsafe = true;
					surfaces.push_back({});
				}
				else
				{
					for (auto &section : overlapping)
					{
						surfaces.push_back
						({
							section.surface->get_surface(),
							rsx::apply_resolution_scale(section.src_x, true),
							rsx::apply_resolution_scale(section.src_y, true),
							rsx::apply_resolution_scale(section.dst_x, true),
							rsx::apply_resolution_scale(section.dst_y, true),
							slice,
							rsx::apply_resolution_scale(section.width, true),
							rsx::apply_resolution_scale(section.height, true)
						});
					}
				}
			}

			if (unsafe)
			{
				//TODO: Gather remaining sides from the texture cache or upload from cpu (too slow?)
				LOG_ERROR(RSX, "Could not gather all required slices for cubemap/3d generation");
			}

			return surfaces;
		}

		template <typename render_target_type, typename surface_store_type>
		sampled_image_descriptor process_framebuffer_resource(commandbuffer_type& cmd, render_target_type texptr, u32 texaddr, u32 gcm_format, surface_store_type& m_rtts,
				u16 tex_width, u16 tex_height, u16 tex_depth, u16 tex_pitch, rsx::texture_dimension_extended extended_dimension, bool is_depth, u32 encoded_remap, const texture_channel_remap_t& decoded_remap)
		{
			const u32 format = gcm_format & ~(CELL_GCM_TEXTURE_UN | CELL_GCM_TEXTURE_LN);
			const auto surface_width = texptr->get_surface_width();
			const auto surface_height = texptr->get_surface_height();

			u32 internal_width = tex_width;
			u32 internal_height = tex_height;
			get_native_dimensions(internal_width, internal_height, texptr);

			if (extended_dimension != rsx::texture_dimension_extended::texture_dimension_2d &&
				extended_dimension != rsx::texture_dimension_extended::texture_dimension_1d)
			{
				if (extended_dimension == rsx::texture_dimension_extended::texture_dimension_cubemap)
				{
					const auto scaled_size = rsx::apply_resolution_scale(internal_width, true);
					if (surface_height == (surface_width * 6))
					{
						return{ texptr->get_surface(), deferred_request_command::cubemap_unwrap, texaddr, format, 0, 0,
								scaled_size, scaled_size, 1,
								texture_upload_context::framebuffer_storage, is_depth, 1.f, 1.f,
								rsx::texture_dimension_extended::texture_dimension_cubemap, decoded_remap };
					}

					sampled_image_descriptor desc = { texptr->get_surface(), deferred_request_command::cubemap_gather, texaddr, format, 0, 0,
							scaled_size, scaled_size, 1,
							texture_upload_context::framebuffer_storage, is_depth, 1.f, 1.f,
							rsx::texture_dimension_extended::texture_dimension_cubemap, decoded_remap };

					auto bpp = get_format_block_size_in_bytes(format);
					desc.external_subresource_desc.sections_to_copy = std::move(gather_texture_slices_from_framebuffers(texaddr, tex_width, tex_height, tex_pitch, 6, bpp, m_rtts));
					return desc;
				}
				else if (extended_dimension == rsx::texture_dimension_extended::texture_dimension_3d && tex_depth > 1)
				{
					auto minimum_height = (tex_height * tex_depth);
					auto scaled_w = rsx::apply_resolution_scale(internal_width, true);
					auto scaled_h = rsx::apply_resolution_scale(internal_height, true);
					if (surface_height >= minimum_height && surface_width >= tex_width)
					{
						return{ texptr->get_surface(), deferred_request_command::_3d_unwrap, texaddr, format, 0, 0,
								scaled_w, scaled_h, tex_depth,
								texture_upload_context::framebuffer_storage, is_depth, 1.f, 1.f,
								rsx::texture_dimension_extended::texture_dimension_3d, decoded_remap };
					}

					sampled_image_descriptor desc = { texptr->get_surface(), deferred_request_command::_3d_gather, texaddr, format, 0, 0,
						scaled_w, scaled_h, tex_depth,
						texture_upload_context::framebuffer_storage, is_depth, 1.f, 1.f,
						rsx::texture_dimension_extended::texture_dimension_3d, decoded_remap };

					const auto bpp = get_format_block_size_in_bytes(format);
					desc.external_subresource_desc.sections_to_copy = std::move(gather_texture_slices_from_framebuffers(texaddr, tex_width, tex_height, tex_pitch, tex_depth, bpp, m_rtts));
					return desc;
				}
			}

			const bool unnormalized = (gcm_format & CELL_GCM_TEXTURE_UN) != 0;
			f32 scale_x = (unnormalized)? (1.f / tex_width) : 1.f;
			f32 scale_y = (unnormalized)? (1.f / tex_height) : 1.f;

			if (extended_dimension == rsx::texture_dimension_extended::texture_dimension_1d)
			{
				internal_height = 1;
				scale_y = 0.f;
			}

			auto bpp = get_format_block_size_in_bytes(format);
			auto overlapping = m_rtts.get_merged_texture_memory_region(texaddr, tex_width, tex_height, tex_pitch, bpp);
			bool requires_merging = false;

			// TODO ruipin: This AUDIT fails due to a bug that kd will have to fix
			//AUDIT( !overlapping.empty() );
			if (overlapping.size() > 1)
			{
				// The returned values are sorted with oldest first and newest last
				// This allows newer data to overwrite older memory when merging the list
				if (overlapping.back().surface == texptr)
				{
					// The texture 'proposed' by the previous lookup is the newest one
					// If it occupies the entire requested region, just use it as-is
					requires_merging = (internal_width > surface_width || internal_height > surface_height);
				}
				else
				{
					requires_merging = true;
				}
			}

			if (requires_merging)
			{
				const auto w = rsx::apply_resolution_scale(internal_width, true);
				const auto h = rsx::apply_resolution_scale(internal_height, true);

				sampled_image_descriptor result = { texptr->get_surface(), deferred_request_command::atlas_gather,
						texaddr, format, 0, 0, w, h, 1, texture_upload_context::framebuffer_storage, is_depth,
						scale_x, scale_y, rsx::texture_dimension_extended::texture_dimension_2d, decoded_remap };

				result.external_subresource_desc.sections_to_copy.reserve(overlapping.size());

				for (auto &section : overlapping)
				{
					result.external_subresource_desc.sections_to_copy.push_back
					({
						section.surface->get_surface(),
						rsx::apply_resolution_scale(section.src_x, true),
						rsx::apply_resolution_scale(section.src_y, true),
						rsx::apply_resolution_scale(section.dst_x, true),
						rsx::apply_resolution_scale(section.dst_y, true),
						0,
						rsx::apply_resolution_scale(section.width, true),
						rsx::apply_resolution_scale(section.height, true)
					});
				}

				return result;
			}

			bool requires_processing = surface_width > internal_width || surface_height > internal_height;
			bool update_subresource_cache = false;
			if (!requires_processing)
			{
				//NOTE: The scale also accounts for sampling outside the RTT region, e.g render to one quadrant but send whole texture for sampling
				//In these cases, internal dimensions will exceed available surface dimensions. Account for the missing information using scaling (missing data will result in border color)
				//TODO: Proper gather and stitching without performance loss
				if (internal_width > surface_width)
					scale_x *= ((f32)internal_width / surface_width);

				if (internal_height > surface_height)
					scale_y *= ((f32)internal_height / surface_height);

				if (!is_depth)
				{
					for (const auto& tex : m_rtts.m_bound_render_targets)
					{
						if (std::get<0>(tex) == texaddr)
						{
							if (g_cfg.video.strict_rendering_mode)
							{
								LOG_WARNING(RSX, "Attempting to sample a currently bound render target @ 0x%x", texaddr);
								requires_processing = true;
								update_subresource_cache = true;
								break;
							}
							else
							{
								//issue a texture barrier to ensure previous writes are visible
								insert_texture_barrier(cmd, texptr);
								break;
							}
						}
					}
				}
				else
				{
					if (texaddr == std::get<0>(m_rtts.m_bound_depth_stencil))
					{
						if (g_cfg.video.strict_rendering_mode)
						{
							LOG_WARNING(RSX, "Attempting to sample a currently bound depth surface @ 0x%x", texaddr);
							requires_processing = true;
							update_subresource_cache = true;
						}
						else
						{
							//issue a texture barrier to ensure previous writes are visible
							insert_texture_barrier(cmd, texptr);
						}
					}
				}
			}

			if (!requires_processing)
			{
				//Check if we need to do anything about the formats
				requires_processing = !render_target_format_is_compatible(texptr, format);
			}

			if (requires_processing)
			{
				const auto w = rsx::apply_resolution_scale(std::min<u16>(internal_width, surface_width), true);
				const auto h = rsx::apply_resolution_scale(std::min<u16>(internal_height, surface_height), true);

				auto command = update_subresource_cache ? deferred_request_command::copy_image_dynamic : deferred_request_command::copy_image_static;
				return { texptr->get_surface(), command, texaddr, format, 0, 0, w, h, 1,
						texture_upload_context::framebuffer_storage, is_depth, scale_x, scale_y,
						rsx::texture_dimension_extended::texture_dimension_2d, decoded_remap };
			}

			return{ texptr->get_view(encoded_remap, decoded_remap), texture_upload_context::framebuffer_storage,
					is_depth, scale_x, scale_y, rsx::texture_dimension_extended::texture_dimension_2d };
		}

		template <typename RsxTextureType, typename surface_store_type, typename ...Args>
		sampled_image_descriptor upload_texture(commandbuffer_type& cmd, RsxTextureType& tex, surface_store_type& m_rtts, Args&&... extras)
		{
			const u32 texaddr = rsx::get_address(tex.offset(), tex.location());
			const u32 tex_size = (u32)get_placed_texture_storage_size(tex, 256, 512);
			const address_range tex_range = address_range::create_start_length(texaddr, tex_size);
			const u32 format = tex.format() & ~(CELL_GCM_TEXTURE_LN | CELL_GCM_TEXTURE_UN);
			const bool is_compressed_format = (format == CELL_GCM_TEXTURE_COMPRESSED_DXT1 || format == CELL_GCM_TEXTURE_COMPRESSED_DXT23 || format == CELL_GCM_TEXTURE_COMPRESSED_DXT45);

			if (!texaddr || !tex_size || !tex_range.valid())
			{
				LOG_ERROR(RSX, "Texture upload requested but texture not found, (address=0x%X, size=0x%X, w=%d, h=%d, p=%d, format=0x%X)", texaddr, tex_size, tex.width(), tex.height(), tex.pitch(), tex.format());
				return {};
			}

			const auto extended_dimension = tex.get_extended_texture_dimension();
			u16 depth = 0;
			u16 tex_height = (u16)tex.height();
			const u16 tex_width = tex.width();
			u16 tex_pitch = is_compressed_format? (u16)(get_texture_size(tex) / tex_height) : tex.pitch(); //NOTE: Compressed textures dont have a real pitch (tex_size = (w*h)/6)
			if (tex_pitch == 0) tex_pitch = tex_width * get_format_block_size_in_bytes(format);

			switch (extended_dimension)
			{
			case rsx::texture_dimension_extended::texture_dimension_1d:
				tex_height = 1;
				depth = 1;
				break;
			case rsx::texture_dimension_extended::texture_dimension_2d:
				depth = 1;
				break;
			case rsx::texture_dimension_extended::texture_dimension_cubemap:
				depth = 6;
				break;
			case rsx::texture_dimension_extended::texture_dimension_3d:
				depth = tex.depth();
				break;
			}

			if (!is_compressed_format)
			{
				//Check for sampleable rtts from previous render passes
				//TODO: When framebuffer Y compression is properly handled, this section can be removed. A more accurate framebuffer storage check exists below this block
				if (auto texptr = m_rtts.get_texture_from_render_target_if_applicable(texaddr))
				{
					if (test_framebuffer(texaddr))
					{
						return process_framebuffer_resource(cmd, texptr, texaddr, tex.format(), m_rtts,
								tex_width, tex_height, depth, tex_pitch, extended_dimension, false, tex.remap(),
								tex.decoded_remap());
					}
					else
					{
						m_rtts.invalidate_surface_address(texaddr, false);
						invalidate_address(texaddr, false, true, std::forward<Args>(extras)...);
					}
				}

				if (auto texptr = m_rtts.get_texture_from_depth_stencil_if_applicable(texaddr))
				{
					if (test_framebuffer(texaddr))
					{
						return process_framebuffer_resource(cmd, texptr, texaddr, tex.format(), m_rtts,
								tex_width, tex_height, depth, tex_pitch, extended_dimension, true, tex.remap(),
								tex.decoded_remap());
					}
					else
					{
						m_rtts.invalidate_surface_address(texaddr, true);
						invalidate_address(texaddr, false, true, std::forward<Args>(extras)...);
					}
				}
			}

			const bool unnormalized = (tex.format() & CELL_GCM_TEXTURE_UN) != 0;
			f32 scale_x = (unnormalized) ? (1.f / tex_width) : 1.f;
			f32 scale_y = (unnormalized) ? (1.f / tex_height) : 1.f;

			if (extended_dimension == rsx::texture_dimension_extended::texture_dimension_1d)
				scale_y = 0.f;

			if (!is_compressed_format)
			{
				// Check if we are re-sampling a subresource of an RTV/DSV texture, bound or otherwise

				const auto rsc = m_rtts.get_surface_subresource_if_applicable(texaddr, tex_width, tex_height, tex_pitch);
				if (rsc.surface)
				{
					if (!test_framebuffer(rsc.base_address))
					{
						m_rtts.invalidate_surface_address(rsc.base_address, rsc.is_depth_surface);
						invalidate_address(rsc.base_address, false, true, std::forward<Args>(extras)...);
					}
					else if (extended_dimension != rsx::texture_dimension_extended::texture_dimension_2d &&
							 extended_dimension != rsx::texture_dimension_extended::texture_dimension_1d)
					{
						LOG_ERROR(RSX, "Sampling of RTT region as non-2D texture! addr=0x%x, Type=%d, dims=%dx%d",
							texaddr, (u8)tex.get_extended_texture_dimension(), tex.width(), tex.height());
					}
					else
					{
						u16 internal_width = tex_width;
						u16 internal_height = tex_height;

						get_native_dimensions(internal_width, internal_height, rsc.surface);
						if (!rsc.x && !rsc.y && rsc.w == internal_width && rsc.h == internal_height)
						{
							//Full sized hit from the surface cache. This should have been already found before getting here
							fmt::throw_exception("Unreachable" HERE);
						}

						internal_width = rsx::apply_resolution_scale(internal_width, true);
						internal_height = (extended_dimension == rsx::texture_dimension_extended::texture_dimension_1d)? 1: rsx::apply_resolution_scale(internal_height, true);

						return{ rsc.surface->get_surface(), deferred_request_command::copy_image_static, rsc.base_address, format,
							rsx::apply_resolution_scale(rsc.x, false), rsx::apply_resolution_scale(rsc.y, false),
							internal_width, internal_height, 1, texture_upload_context::framebuffer_storage, rsc.is_depth_surface, scale_x, scale_y,
							rsx::texture_dimension_extended::texture_dimension_2d, tex.decoded_remap() };
					}
				}
			}

			{
				//Search in cache and upload/bind
				reader_lock lock(m_cache_mutex);

				auto cached_texture = find_texture_from_dimensions(texaddr, tex_width, tex_height, depth);
				if (cached_texture)
				{
					//TODO: Handle invalidated framebuffer textures better. This is awful
					if (cached_texture->get_context() == rsx::texture_upload_context::framebuffer_storage)
					{
						if (!cached_texture->is_locked())
						{
							lock.upgrade();
							cached_texture->set_dirty(true);
							m_unreleased_texture_objects++;
						}
					}
					else
					{
						if (cached_texture->get_image_type() == rsx::texture_dimension_extended::texture_dimension_1d)
							scale_y = 0.f;

						return{ cached_texture->get_view(tex.remap(), tex.decoded_remap()), cached_texture->get_context(), cached_texture->is_depth_texture(), scale_x, scale_y, cached_texture->get_image_type() };
					}
				}

				if (is_hw_blit_engine_compatible(format))
				{
					//Find based on range instead
					auto overlapping_surfaces = find_texture_from_range(tex_range);
					if (!overlapping_surfaces.empty())
					{
						for (const auto &surface : overlapping_surfaces)
						{
							if (surface->get_context() != rsx::texture_upload_context::blit_engine_dst ||
								!surface->overlaps(tex_range, rsx::section_bounds::confirmed_range))
								continue;

							if (surface->get_width() >= tex_width && surface->get_height() >= tex_height)
							{
								u16 offset_x = 0, offset_y = 0;
								if (const u32 address_offset = texaddr - surface->get_section_base())
								{
									const auto bpp = get_format_block_size_in_bytes(format);
									offset_y = address_offset / tex_pitch;
									offset_x = (address_offset % tex_pitch) / bpp;
								}

								if ((offset_x + tex_width) <= surface->get_width() &&
									(offset_y + tex_height) <= surface->get_height())
								{
									if (extended_dimension != rsx::texture_dimension_extended::texture_dimension_2d &&
										extended_dimension != rsx::texture_dimension_extended::texture_dimension_1d)
									{
										LOG_ERROR(RSX, "Texture resides in blit engine memory, but requested type is not 2D (%d)", (u32)extended_dimension);
										break;
									}

									auto src_image = surface->get_raw_texture();
									return{ src_image, deferred_request_command::copy_image_static, surface->get_section_base(), format, offset_x, offset_y, tex_width, tex_height, 1,
										texture_upload_context::blit_engine_dst, surface->is_depth_texture(), scale_x, scale_y, rsx::texture_dimension_extended::texture_dimension_2d,
										rsx::default_remap_vector };
								}
							}
						}
					}
				}

				//Do direct upload from CPU as the last resort
				const bool is_swizzled = !(tex.format() & CELL_GCM_TEXTURE_LN);
				auto subresources_layout = get_subresources_layout(tex);

				bool is_depth_format = false;
				switch (format)
				{
				case CELL_GCM_TEXTURE_DEPTH16:
				case CELL_GCM_TEXTURE_DEPTH16_FLOAT:
				case CELL_GCM_TEXTURE_DEPTH24_D8:
				case CELL_GCM_TEXTURE_DEPTH24_D8_FLOAT:
					is_depth_format = true;
					break;
				}

				// Upgrade lock
				lock.upgrade();

				//Invalidate with writing=false, discard=false, rebuild=false, native_flush=true
				invalidate_range_impl_base(tex_range, false, false, true, std::forward<Args>(extras)...);

				//NOTE: SRGB correction is to be handled in the fragment shader; upload as linear RGB
				m_texture_memory_in_use += (tex_pitch * tex_height);
				return{ upload_image_from_cpu(cmd, texaddr, tex_width, tex_height, depth, tex.get_exact_mipmap_count(), tex_pitch, format,
					texture_upload_context::shader_read, subresources_layout, extended_dimension, is_swizzled)->get_view(tex.remap(), tex.decoded_remap()),
					texture_upload_context::shader_read, is_depth_format, scale_x, scale_y, extended_dimension };
			}
		}

		template <typename surface_store_type, typename blitter_type, typename ...Args>
		blit_op_result upload_scaled_image(rsx::blit_src_info& src, rsx::blit_dst_info& dst, bool interpolate, commandbuffer_type& cmd, surface_store_type& m_rtts, blitter_type& blitter, Args&&... extras)
		{
			//Since we will have dst in vram, we can 'safely' ignore the swizzle flag
			//TODO: Verify correct behavior
			bool src_is_render_target = false;
			bool dst_is_render_target = false;
			bool dst_is_argb8 = (dst.format == rsx::blit_engine::transfer_destination_format::a8r8g8b8);
			bool src_is_argb8 = (src.format == rsx::blit_engine::transfer_source_format::a8r8g8b8);

			typeless_xfer typeless_info = {};
			image_resource_type vram_texture = 0;
			image_resource_type dest_texture = 0;

			const u32 src_address = (u32)((u64)src.pixels - (u64)vm::base(0));
			const u32 dst_address = (u32)((u64)dst.pixels - (u64)vm::base(0));

			f32 scale_x = dst.scale_x;
			f32 scale_y = dst.scale_y;

			//Offset in x and y for src is 0 (it is already accounted for when getting pixels_src)
			//Reproject final clip onto source...
			u16 src_w = (u16)((f32)dst.clip_width / scale_x);
			u16 src_h = (u16)((f32)dst.clip_height / scale_y);

			u16 dst_w = dst.clip_width;
			u16 dst_h = dst.clip_height;

			if (!src_w || !src_h || !dst_w || !dst_h)
			{
				LOG_ERROR(RSX, "Blit engine request failed because of empty region");
				return true;
			}

			//Check if src/dst are parts of render targets
			auto dst_subres = m_rtts.get_surface_subresource_if_applicable(dst_address, dst.width, dst.clip_height, dst.pitch, true, false, false);
			dst_is_render_target = dst_subres.surface != nullptr;

			if (dst_is_render_target && dst_subres.surface->get_native_pitch() != dst.pitch)
			{
				//Surface pitch is invalid if it is less that the rsx pitch (usually set to 64 in such a case)
				if (dst_subres.surface->get_rsx_pitch() != dst.pitch ||
					dst.pitch < dst_subres.surface->get_native_pitch())
					dst_is_render_target = false;
			}

			//TODO: Handle cases where src or dst can be a depth texture while the other is a color texture - requires a render pass to emulate
			auto src_subres = m_rtts.get_surface_subresource_if_applicable(src_address, src_w, src_h, src.pitch, true, false, false);
			src_is_render_target = src_subres.surface != nullptr;

			if (src_is_render_target && src_subres.surface->get_native_pitch() != src.pitch)
			{
				//Surface pitch is invalid if it is less that the rsx pitch (usually set to 64 in such a case)
				if (src_subres.surface->get_rsx_pitch() != src.pitch ||
					src.pitch < src_subres.surface->get_native_pitch())
					src_is_render_target = false;
			}

			if (src_is_render_target && !test_framebuffer(src_subres.base_address))
			{
				m_rtts.invalidate_surface_address(src_subres.base_address, src_subres.is_depth_surface);
				invalidate_address(src_subres.base_address, false, true, std::forward<Args>(extras)...);
				src_is_render_target = false;
			}

			if (dst_is_render_target && !test_framebuffer(dst_subres.base_address))
			{
				m_rtts.invalidate_surface_address(dst_subres.base_address, dst_subres.is_depth_surface);
				invalidate_address(dst_subres.base_address, false, true, std::forward<Args>(extras)...);
				dst_is_render_target = false;
			}

			//Always use GPU blit if src or dst is in the surface store
			if (!g_cfg.video.use_gpu_texture_scaling && !(src_is_render_target || dst_is_render_target))
				return false;

			if (src_is_render_target)
			{
				const auto surf = src_subres.surface;
				auto src_bpp = surf->get_native_pitch() / surf->get_surface_width();
				auto expected_bpp = src_is_argb8 ? 4 : 2;
				if (src_bpp != expected_bpp)
				{
					//Enable type scaling in src
					typeless_info.src_is_typeless = true;
					typeless_info.src_is_depth = src_subres.is_depth_surface;
					typeless_info.src_scaling_hint = (f32)src_bpp / expected_bpp;
					typeless_info.src_gcm_format = src_is_argb8 ? CELL_GCM_TEXTURE_A8R8G8B8 : CELL_GCM_TEXTURE_R5G6B5;

					src_w = (u16)(src_w / typeless_info.src_scaling_hint);
					if (!src_subres.is_clipped)
						src_subres.w = (u16)(src_subres.w / typeless_info.src_scaling_hint);
					else
						src_subres = m_rtts.get_surface_subresource_if_applicable(src_address, src_w, src_h, src.pitch, true, false, false);

					verify(HERE), src_subres.surface != nullptr;
				}
			}

			if (dst_is_render_target)
			{
				auto dst_bpp = dst_subres.surface->get_native_pitch() / dst_subres.surface->get_surface_width();
				auto expected_bpp = dst_is_argb8 ? 4 : 2;
				if (dst_bpp != expected_bpp)
				{
					//Enable type scaling in dst
					typeless_info.dst_is_typeless = true;
					typeless_info.dst_is_depth = dst_subres.is_depth_surface;
					typeless_info.dst_scaling_hint = (f32)dst_bpp / expected_bpp;
					typeless_info.dst_gcm_format = dst_is_argb8 ? CELL_GCM_TEXTURE_A8R8G8B8 : CELL_GCM_TEXTURE_R5G6B5;

					dst_w = (u16)(dst_w / typeless_info.dst_scaling_hint);
					if (!dst_subres.is_clipped)
						dst_subres.w = (u16)(dst_subres.w / typeless_info.dst_scaling_hint);
					else
						dst_subres = m_rtts.get_surface_subresource_if_applicable(dst_address, dst_w, dst_h, dst.pitch, true, false, false);

					verify(HERE), dst_subres.surface != nullptr;
				}
			}

			reader_lock lock(m_cache_mutex);

			//Check if trivial memcpy can perform the same task
			//Used to copy programs to the GPU in some cases
			if (!src_is_render_target && !dst_is_render_target && dst_is_argb8 == src_is_argb8 && !dst.swizzled)
			{
				if ((src.slice_h == 1 && dst.clip_height == 1) ||
					(dst.clip_width == src.width && dst.clip_height == src.slice_h && src.pitch == dst.pitch))
				{
					const u8 bpp = dst_is_argb8 ? 4 : 2;
					const u32 memcpy_bytes_length = dst.clip_width * bpp * dst.clip_height;

					lock.upgrade();
					invalidate_range_impl_base(address_range::create_start_length(src_address, memcpy_bytes_length), false, false, true, std::forward<Args>(extras)...);
					invalidate_range_impl_base(address_range::create_start_length(dst_address, memcpy_bytes_length), true, false, true, std::forward<Args>(extras)...);
					memcpy(dst.pixels, src.pixels, memcpy_bytes_length);
					return true;
				}
			}

			u16 max_dst_width = dst.width;
			u16 max_dst_height = dst.height;
			areai src_area = { 0, 0, src_w, src_h };
			areai dst_area = { 0, 0, dst_w, dst_h };

			//1024 height is a hack (for ~720p buffers)
			//It is possible to have a large buffer that goes up to around 4kx4k but anything above 1280x720 is rare
			//RSX only handles 512x512 tiles so texture 'stitching' will eventually be needed to be completely accurate
			//Sections will be submitted as (512x512 + 512x512 + 256x512 + 512x208 + 512x208 + 256x208) to blit a 720p surface to the backbuffer for example
			size2i dst_dimensions = { dst.pitch / (dst_is_argb8 ? 4 : 2), dst.height };
			if (src_is_render_target)
			{
				if (dst_dimensions.width == src_subres.surface->get_surface_width())
					dst_dimensions.height = std::max(src_subres.surface->get_surface_height(), dst.height);
				else if (dst.max_tile_h > dst.height)
					dst_dimensions.height = std::min((s32)dst.max_tile_h, 1024);
			}

			section_storage_type* cached_dest = nullptr;
			bool invalidate_dst_range = false;

			if (!dst_is_render_target)
			{
				// Check for any available region that will fit this one
				auto overlapping_surfaces = find_texture_from_range(address_range::create_start_length(dst_address, dst.pitch * dst.clip_height));

				for (const auto &surface : overlapping_surfaces)
				{
					if (surface->get_context() != rsx::texture_upload_context::blit_engine_dst)
						continue;

					if (surface->get_rsx_pitch() != dst.pitch)
						continue;

					const auto old_dst_area = dst_area;
					if (const u32 address_offset = dst_address - surface->get_section_base())
					{
						const u16 bpp = dst_is_argb8 ? 4 : 2;
						const u16 offset_y = address_offset / dst.pitch;
						const u16 offset_x = address_offset % dst.pitch;
						const u16 offset_x_in_block = offset_x / bpp;

						dst_area.x1 += offset_x_in_block;
						dst_area.x2 += offset_x_in_block;
						dst_area.y1 += offset_y;
						dst_area.y2 += offset_y;
					}

					// Validate clipping region
					if ((unsigned)dst_area.x2 <= surface->get_width() &&
						(unsigned)dst_area.y2 <= surface->get_height())
					{
						cached_dest = surface;
						break;
					}

					dst_area = old_dst_area;
				}

				if (cached_dest)
				{
					dest_texture = cached_dest->get_raw_texture();
					typeless_info.dst_context = cached_dest->get_context();

					max_dst_width = cached_dest->get_width();
					max_dst_height = cached_dest->get_height();
				}
				else if (overlapping_surfaces.size() > 0)
				{
					invalidate_dst_range = true;
				}
			}
			else
			{
				//TODO: Investigate effects of tile compression

				dst_area.x1 = dst_subres.x;
				dst_area.y1 = dst_subres.y;
				dst_area.x2 += dst_subres.x;
				dst_area.y2 += dst_subres.y;

				dest_texture = dst_subres.surface->get_surface();
				typeless_info.dst_context = texture_upload_context::framebuffer_storage;

				max_dst_width = (u16)(dst_subres.surface->get_surface_width() * typeless_info.dst_scaling_hint);
				max_dst_height = dst_subres.surface->get_surface_height();
			}

			//Create source texture if does not exist
			if (!src_is_render_target)
			{
				auto overlapping_surfaces = find_texture_from_range(address_range::create_start_length(src_address, src.pitch * src.height));

				auto old_src_area = src_area;
				for (const auto &surface : overlapping_surfaces)
				{
					//look for any that will fit, unless its a shader read surface or framebuffer_storage
					if (surface->get_context() == rsx::texture_upload_context::shader_read ||
						surface->get_context() == rsx::texture_upload_context::framebuffer_storage)
						continue;

					if (surface->get_rsx_pitch() != src.pitch)
						continue;

					if (const u32 address_offset = src_address - surface->get_section_base())
					{
						const u16 bpp = src_is_argb8 ? 4 : 2;
						const u16 offset_y = address_offset / src.pitch;
						const u16 offset_x = address_offset % src.pitch;
						const u16 offset_x_in_block = offset_x / bpp;

						src_area.x1 += offset_x_in_block;
						src_area.x2 += offset_x_in_block;
						src_area.y1 += offset_y;
						src_area.y2 += offset_y;
					}

					if (src_area.x2 <= surface->get_width() &&
						src_area.y2 <= surface->get_height())
					{
						vram_texture = surface->get_raw_texture();
						typeless_info.src_context = surface->get_context();
						break;
					}

					src_area = old_src_area;
				}

				if (!vram_texture)
				{
					lock.upgrade();

					invalidate_range_impl_base(address_range::create_start_length(src_address, src.pitch * src.slice_h), false, false, true, std::forward<Args>(extras)...);

					const u16 pitch_in_block = src_is_argb8 ? src.pitch >> 2 : src.pitch >> 1;
					std::vector<rsx_subresource_layout> subresource_layout;
					rsx_subresource_layout subres = {};
					subres.width_in_block = src.width;
					subres.height_in_block = src.slice_h;
					subres.pitch_in_block = pitch_in_block;
					subres.depth = 1;
					subres.data = { (const gsl::byte*)src.pixels, src.pitch * src.slice_h };
					subresource_layout.push_back(subres);

					const u32 gcm_format = src_is_argb8 ? CELL_GCM_TEXTURE_A8R8G8B8 : CELL_GCM_TEXTURE_R5G6B5;
					vram_texture = upload_image_from_cpu(cmd, src_address, src.width, src.slice_h, 1, 1, src.pitch, gcm_format, texture_upload_context::blit_engine_src,
						subresource_layout, rsx::texture_dimension_extended::texture_dimension_2d, dst.swizzled)->get_raw_texture();

					m_texture_memory_in_use += src.pitch * src.slice_h;
					typeless_info.src_context = texture_upload_context::blit_engine_src;
				}
			}
			else
			{
				if (!dst_is_render_target)
				{
					u16 src_subres_w = src_subres.w;
					u16 src_subres_h = src_subres.h;
					get_rsx_dimensions(src_subres_w, src_subres_h, src_subres.surface);

					const int dst_width = (int)(src_subres_w * scale_x * typeless_info.src_scaling_hint);
					const int dst_height = (int)(src_subres_h * scale_y);

					dst_area.x2 = dst_area.x1 + dst_width;
					dst_area.y2 = dst_area.y1 + dst_height;
				}

				src_area.x2 = src_subres.w;
				src_area.y2 = src_subres.h;

				src_area.x1 = src_subres.x;
				src_area.y1 = src_subres.y;
				src_area.x2 += src_subres.x;
				src_area.y2 += src_subres.y;

				vram_texture = src_subres.surface->get_surface();
				typeless_info.src_context = texture_upload_context::framebuffer_storage;
			}

			const bool src_is_depth = src_subres.is_depth_surface;
			const bool dst_is_depth = dst_is_render_target? dst_subres.is_depth_surface :
										dest_texture ? cached_dest->is_depth_texture() : src_is_depth;

			//Type of blit decided by the source, destination use should adapt on the fly
			const bool is_depth_blit = src_is_depth;

			bool format_mismatch = (src_is_depth != dst_is_depth);
			if (format_mismatch)
			{
				if (dst_is_render_target)
				{
					LOG_ERROR(RSX, "Depth<->RGBA blit on a framebuffer requested but not supported");
					return false;
				}
			}
			else if (src_is_render_target && cached_dest)
			{
				switch (cached_dest->get_gcm_format())
				{
				case CELL_GCM_TEXTURE_A8R8G8B8:
				case CELL_GCM_TEXTURE_DEPTH24_D8:
					format_mismatch = !dst_is_argb8;
					break;
				case CELL_GCM_TEXTURE_R5G6B5:
				case CELL_GCM_TEXTURE_DEPTH16:
					format_mismatch = dst_is_argb8;
					break;
				default:
					format_mismatch = true;
					break;
				}
			}

			//TODO: Check for other types of format mismatch
			const address_range dst_range = address_range::create_start_length(dst_address, dst.pitch * dst.height);
			AUDIT( cached_dest == nullptr || cached_dest->overlaps(dst_range, section_bounds::full_range) );
			if (format_mismatch)
			{
				lock.upgrade();

				// Invalidate as the memory is not reusable now
				const address_range &cached_range = cached_dest->get_section_range();
				const address_range inv_range = invalidate_dst_range ? dst_range.get_min_max(cached_range) : cached_range;

				invalidate_range_impl_base(inv_range, true, false, true, std::forward<Args>(extras)...);
				AUDIT(!cached_dest->is_locked());

				dest_texture = 0;
				cached_dest = nullptr;
			}
			else if (invalidate_dst_range)
			{
				lock.upgrade();
				invalidate_range_impl_base(dst_range, true, false, true, std::forward<Args>(extras)...);
			}

			u32 gcm_format;
			if (is_depth_blit)
				gcm_format = (dst_is_argb8) ? CELL_GCM_TEXTURE_DEPTH24_D8 : CELL_GCM_TEXTURE_DEPTH16;
			else
				gcm_format = (dst_is_argb8) ? CELL_GCM_TEXTURE_A8R8G8B8 : CELL_GCM_TEXTURE_R5G6B5;

			if (cached_dest)
			{
				//Prep surface
				auto channel_order = src_is_render_target ? rsx::texture_create_flags::native_component_order :
					dst_is_argb8 ? rsx::texture_create_flags::default_component_order :
					rsx::texture_create_flags::swapped_native_component_order;

				enforce_surface_creation_type(*cached_dest, gcm_format, channel_order);
			}

			//Validate clipping region
			if ((dst.offset_x + dst.clip_x + dst.clip_width) > max_dst_width) dst.clip_x = 0;
			if ((dst.offset_y + dst.clip_y + dst.clip_height) > max_dst_height) dst.clip_y = 0;

			//Reproject clip offsets onto source to simplify blit
			if (dst.clip_x || dst.clip_y)
			{
				const u16 scaled_clip_offset_x = (const u16)((f32)dst.clip_x / (scale_x * typeless_info.src_scaling_hint));
				const u16 scaled_clip_offset_y = (const u16)((f32)dst.clip_y / scale_y);

				src_area.x1 += scaled_clip_offset_x;
				src_area.x2 += scaled_clip_offset_x;
				src_area.y1 += scaled_clip_offset_y;
				src_area.y2 += scaled_clip_offset_y;
			}

			if (dest_texture == 0)
			{
				lock.upgrade();

				//render target data is already in correct swizzle layout
				auto channel_order = src_is_render_target ? rsx::texture_create_flags::native_component_order :
					dst_is_argb8 ? rsx::texture_create_flags::default_component_order :
					rsx::texture_create_flags::swapped_native_component_order;

				//NOTE: Should upload from cpu instead of creating a blank texture
				const address_range dst_range = address_range::create_start_length(dst.rsx_address, dst.pitch * dst_dimensions.height);
				cached_dest = create_new_texture(cmd, dst_range, dst_dimensions.width, dst_dimensions.height, 1, 1,
					gcm_format, rsx::texture_upload_context::blit_engine_dst, rsx::texture_dimension_extended::texture_dimension_2d,
					channel_order);

				dest_texture = cached_dest->get_raw_texture();
				typeless_info.dst_context = texture_upload_context::blit_engine_dst;
				m_texture_memory_in_use += dst_range.length();
			}

			if (cached_dest)
			{
				lock.upgrade();

				if (cached_dest->is_locked() && cached_dest->is_synchronized())
				{
					// Premature readback
					m_num_cache_mispredictions++;
				}

				u32 mem_length;
				const u32 mem_base = dst_address - cached_dest->get_section_base();

				if (dst.clip_height == 1)
				{
					mem_length = dst.clip_width * (dst_is_argb8 ? 4 : 2);
				}
				else
				{
					const u32 mem_excess = mem_base % dst.pitch;
					mem_length = (dst.pitch * dst.clip_height) - mem_excess;
				}

				verify(HERE), (mem_base + mem_length) <= cached_dest->get_section_size();

				cached_dest->reprotect(utils::protection::no, { mem_base, mem_length });
				cached_dest->touch();
			}
			else
			{
				verify(HERE), dst_is_render_target;
			}

			if (rsx::get_resolution_scale_percent() != 100)
			{
				const f32 resolution_scale = rsx::get_resolution_scale();
				if (src_is_render_target)
				{
					if (src_subres.surface->get_surface_width() > g_cfg.video.min_scalable_dimension)
					{
						src_area.x1 = (u16)(src_area.x1 * resolution_scale);
						src_area.x2 = (u16)(src_area.x2 * resolution_scale);
					}

					if (src_subres.surface->get_surface_height() > g_cfg.video.min_scalable_dimension)
					{
						src_area.y1 = (u16)(src_area.y1 * resolution_scale);
						src_area.y2 = (u16)(src_area.y2 * resolution_scale);
					}
				}

				if (dst_is_render_target)
				{
					if (dst_subres.surface->get_surface_width() > g_cfg.video.min_scalable_dimension)
					{
						dst_area.x1 = (u16)(dst_area.x1 * resolution_scale);
						dst_area.x2 = (u16)(dst_area.x2 * resolution_scale);
					}

					if (dst_subres.surface->get_surface_height() > g_cfg.video.min_scalable_dimension)
					{
						dst_area.y1 = (u16)(dst_area.y1 * resolution_scale);
						dst_area.y2 = (u16)(dst_area.y2 * resolution_scale);
					}
				}
			}

			typeless_info.analyse();
			blitter.scale_image(vram_texture, dest_texture, src_area, dst_area, interpolate, is_depth_blit, typeless_info);
			notify_surface_changed(dst.rsx_address);

			blit_op_result result = true;
			result.is_depth = is_depth_blit;

			if (cached_dest)
			{
				result.real_dst_address = cached_dest->get_section_base();
				result.real_dst_size = cached_dest->get_section_size();
			}
			else
			{
				result.real_dst_address = dst.rsx_address;
				result.real_dst_size = dst.pitch * dst_dimensions.height;
			}

			return result;
		}

		void do_update()
		{
			if (m_flush_always_cache.size())
			{
				if (m_cache_update_tag.load(std::memory_order_consume) != m_flush_always_update_timestamp)
				{
					std::lock_guard lock(m_cache_mutex);
					bool update_tag = false;

					for (const auto &It : m_flush_always_cache)
					{
						auto& section = find_cached_texture(address_range::create_start_length(It.first, It.second));
						if (section.get_protection() != utils::protection::no)
						{
							verify(HERE), section.exists();
							AUDIT(section.get_memory_read_flags() == memory_read_flags::flush_always);

							section.reprotect(utils::protection::no);
							tag_framebuffer(It.first);
							update_tag = true;
						}
					}

					if (update_tag) update_cache_tag();
					m_flush_always_update_timestamp = m_cache_update_tag.load(std::memory_order_consume);
				}
			}
		}

		void reset_frame_statistics()
		{
			m_num_flush_requests.store(0u);
			m_num_cache_misses.store(0u);
			m_num_cache_mispredictions.store(0u);
			m_num_cache_speculative_writes.store(0u);
		}

		virtual const u32 get_unreleased_textures_count() const
		{
			return m_unreleased_texture_objects;
		}

		virtual const u32 get_texture_memory_in_use() const
		{
			return m_texture_memory_in_use;
		}

		virtual u32 get_num_flush_requests() const
		{
			return m_num_flush_requests;
		}

		virtual u32 get_num_cache_mispredictions() const
		{
			return m_num_cache_mispredictions;
		}

		virtual u32 get_num_cache_speculative_writes() const
		{
			return m_num_cache_speculative_writes;
		}

		virtual f32 get_cache_miss_ratio() const
		{
			const auto num_flushes = m_num_flush_requests.load();
			return (num_flushes == 0u) ? 0.f : (f32)m_num_cache_misses.load() / num_flushes;
		}

		/**
		 * The read only texture invalidate flag is set if a read only texture is trampled by framebuffer memory
		 * If set, all cached read only textures are considered invalid and should be re-fetched from the texture cache
		 */
		virtual void clear_ro_tex_invalidate_intr()
		{
			read_only_tex_invalidate = false;
		}

		virtual bool get_ro_tex_invalidate_intr() const
		{
			return read_only_tex_invalidate;
		}

		void tag_framebuffer(u32 texaddr)
		{
			auto super_ptr = rsx::get_super_ptr(texaddr, 4);
			volatile u32 *ptr = super_ptr.get<volatile u32>();
			*ptr = texaddr;
			super_ptr.flush(0, 4);
		}

		bool test_framebuffer(u32 texaddr)
		{
			volatile const u32 *ptr = rsx::get_super_ptr(texaddr, 4).get<volatile const u32>();
			return *ptr == texaddr;
		}
	};
}
